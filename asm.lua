local ffi = require "ffi"
local l_type = type
local function type(v)
    local vt = l_type(v)
    if vt == "table" then
        local vmt = getmetatable(v)
        if vmt and vmt.__name then
            return vmt.__name
        end
    end

    return vt
end

--We must properly get the stderr and stdout from the executed programmes so we cant use os.execute or io.popen
--we gotta pipe!
ffi.cdef [[
    typedef int pid_t;
    typedef unsigned int size_t;
    typedef long ssize_t;

    int pipe(int pipefd[2]);
    pid_t fork(void);
    int dup2(int oldfd, int newfd);
    int execl(const char *path, const char *arg, ...);
    pid_t waitpid(pid_t pid, int *wstatus, int options);
    ssize_t read(int fd, void *buf, size_t count);
    int close(int fd);
]]

---@param command string
---@return string
local function execute(command)
    local stdout_pipe = ffi.new("int[2]")
    local stderr_pipe = ffi.new("int[2]")

    if ffi.C.pipe(stdout_pipe) == -1 or ffi.C.pipe(stderr_pipe) == -1 then
        error("Failed to create pipes")
    end

    local pid = ffi.C.fork()
    if pid == -1 then
        error("Failed to fork")
    end

    if pid == 0 then
        -- Child process
        ffi.C.close(stdout_pipe[0])
        ffi.C.close(stderr_pipe[0])
        ffi.C.dup2(stdout_pipe[1], 1)
        ffi.C.dup2(stderr_pipe[1], 2)
        ffi.C.close(stdout_pipe[1])
        ffi.C.close(stderr_pipe[1])
        ffi.C.execl("/bin/sh", "sh", "-c", command, nil)
        ffi.C._exit(1)
    end
    -- Parent process
    ffi.C.close(stdout_pipe[1])
    ffi.C.close(stderr_pipe[1])
    local buffer_size = 4096
    local buffer = ffi.new("char[?]", buffer_size)
    local function read_pipe(pipe_fd)
        local output = {}
        while true do
            local bytes_read = ffi.C.read(pipe_fd, buffer, buffer_size)
            if bytes_read <= 0 then break end
            table.insert(output, ffi.string(buffer, bytes_read))
        end
        return table.concat(output)
    end
    local stdout_output = read_pipe(stdout_pipe[0])
    local stderr_output = read_pipe(stderr_pipe[0])
    ffi.C.close(stdout_pipe[0])
    ffi.C.close(stderr_pipe[0])
    local status = ffi.new("int[1]")
    ffi.C.waitpid(pid, status, 0)
    local ret_code = status[0]

    local success = (ret_code == 0)
    if not success then
        error("Command failed with code " .. ret_code..":\n\x1b[31m"..stderr_output.."\x1b[0m")
    end

    return stdout_output
end

---@param tbl table
---@param sep string?
---@return string
local function concat_table(tbl, sep)
    local str = ""

    for i, v in ipairs(tbl) do
        if i > 1 then
            str = str..(sep or "")
        end

        str = str..tostring(v)
    end

    return str
end

---@generic T
---@param from any[]
---@param to T
---@return T
local function append_instructions(from, to)
    for _, instr in ipairs(from) do
        local ty = type(instr)
        if ty == "Instruction" then
            table.insert(to, instr)
        elseif ty == "table" then
            append_instructions(instr, to)
        else
            error("Invalid instruction type: " .. ty)
        end
    end

    return to
end

---@param cb fun(label: (fun(lbl: string): fun(ctx: table)), instrs: table, reg: table, extern: fun(s: string))
---@param dump boolean?
---@return { [string] : function }
local function asm(cb, dump)
    local tmp_in = string.format("tmp.%d.asm", math.floor(os.time() / 1000))
    local f = assert(io.open(tmp_in, "w"))
    f:write("[SECTION .text]\n")

    local instrs = setmetatable({}, {
        __index = function (self, key)
            return setmetatable({ name = key, params = {} }, {
                __call = function (self, p1, p2)
                    return setmetatable({ name = self.name, params = {p1, p2} }, getmetatable(self))
                end,

                __tostring = function (self)
                    return self.name.." "..concat_table(self.params, ", ")
                end,

                __name = "Instruction"
            })
        end
    })

    local reg = setmetatable({}, {
        __index = function (self, key)
            return setmetatable({ name = key }, {
                __tostring = function (self)
                    return self.name
                end,

                __name = "Register"
            })
        end
    })

    local function label(name)
        return function (instrs)
            f:write("[GLOBAL _"..name.."]\n")
            f:write("_"..name..":\n")
            local flat_instrs = append_instructions(instrs, {})

            for _, instr in ipairs(flat_instrs) do
                f:write("    "..tostring(instr).."\n")
            end
        end
    end

    local function extern(sym)
        f:write("[extern _"..sym.."]\n")
    end

    cb(label, instrs, reg, extern)

    f:close()

    local tmp_out = os.tmpname()
    execute("nasm -f macho64 "..tmp_in.." -o "..tmp_out)

    local dylib_path = os.tmpname()
    execute("clang -shared "..tmp_out.." -o "..dylib_path)

    if not dump then
        os.remove(tmp_in)
    end
    os.remove(tmp_out)
    local lib = ffi.load(dylib_path)

    return setmetatable({}, {
        __index = function (self, def)
            def = def..";"
            ffi.cdef(def)
            --get the symbol name from the def like uint64_t add(uint64_t a, uint64_t b) would get add
            local symbol = def:match("([%w_]+)%s*%(")
            return lib[symbol]
        end
    })
end

--

local lib = asm(function (label, instructions, reg, extern)
    local MOV, ADD, PUSH, POP, LEAVE, RET, SYSCALL = instructions.MOV, instructions.ADD, instructions.PUSH, instructions.POP, instructions.LEAVE, instructions.RET, instructions.SYSCALL
    local function frame(code)
        return {
            PUSH(reg.RBP);
            MOV(reg.RBP, reg.RSP);

            code;

            LEAVE;
            RET;
        }
    end

    local function func(called)
        return function(instrs)
            return label(called)(frame(instrs))
        end
    end

    --function add(a: qword, b: qword): qword
    func "add" {
        MOV(reg.RAX, reg.RDI);
        ADD(reg.RAX, reg.RSI);
    }

    --function write_out(RDI s: byte[len], RSI len: qword): qword
    func "write_out" {
        PUSH(reg.RSI);
        PUSH(reg.RDI);
        MOV(reg.RAX, 0x2000004);    --write
        MOV(reg.RDI, 1);            --stdout
        POP(reg.RSI);               --restore the RSI register, this is the pointer to the string
        POP(reg.RDX);               --restore the RDX register, this is the length of the string
        SYSCALL;
    }
end, true)

local add = lib["uint64_t add(uint64_t a, uint64_t b)"]
local write_out = lib["uint64_t write_out(const char *s, uint64_t len)"]

---@param str string
---@return integer
local function wrstr(str) return write_out(str, #str) end

print(add(1, 2)) --> 3

wrstr "Hello, world!\n"
