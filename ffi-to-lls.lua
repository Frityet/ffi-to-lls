#!/usr/bin/env luajit

-- Copyright (C) 2024 Amrit Bhogal
--
-- This file is part of FFI-to-LLS.
--
-- FFI-to-LLS is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- FFI-to-LLS is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with FFI-to-LLS.  If not, see <https://www.gnu.org/licenses/>.

local ffi = require("ffi")
local utilities = require("utilities")
---@param msg string
local function warn(msg) return io.stderr:write(msg.."\n") end

---@type clang
local clang do
    local ok, lcdef = pcall(require, "libclang-def")
    if ok then
        clang = lcdef
    else
        local inc_dir = assert(io.popen("llvm-config --includedir", "r")):read("*a")
        if inc_dir:sub(-1) == "\n" then
            inc_dir = inc_dir:sub(1, -2)
        end

        warn("Generating libclang-def.lua using headers from "..inc_dir)
        local header_contents = utilities.preprocess_header(inc_dir.."/clang-c/Index.h", {
            "-I"..inc_dir,
        })
        local f = assert(io.open("libclang-def.lua", "w"))
        f:write(string.format([==[
            local ffi = require("ffi")
            ffi.cdef [=[
                typedef uint64_t time_t;

                %s
            ]=]

            local llvm_lib_path do
                local f = assert(io.popen("llvm-config --libdir", "r"))
                llvm_lib_path = f:read("*a")
                if llvm_lib_path:sub(-1) == "\n" then
                    llvm_lib_path = llvm_lib_path:sub(1, -2)
                end
            end


            return setmetatable({ _lib = ffi.load(llvm_lib_path.."/"..(ffi.os == "Windows" and "libclang.dll" or (ffi.os == "OSX" and "libclang.dylib" or "libclang.so"))) }, {
                __index = function(self, key)
                    local ok, sym = pcall(function()
                        return self._lib[key]
                    end)

                    if not ok then
                        return self._lib["clang_"..key]
                    else
                       return sym
                    end
                end,
                __newindex = function(self, key, value)
                    local ok, sym = pcall(function()
                        self._lib[key] = value
                    end)

                    if not ok then
                        self._lib["clang_"..key] = value
                    end
                end
            })
        ]==], header_contents:gsub("#line[^\n]*\n", ""):gsub("#include[^\n]*\n", "")))
        f:close()
    end

    clang = require("libclang-def")
end

ffi.cdef [[
    unsigned int visit_cursor(CXCursor cursor, int (*vis)(CXCursor *cursor, CXCursor *parent));
]]

ffi.metatype("CXString", {
    __tostring = function(self)
        return ffi.string(clang.getCString(self))
    end,

    __concat = function(a, b)
        return tostring(a)..tostring(b)
    end,

    __gc = clang.disposeString
})

---@type string?, string?
local in_path, out_path = arg[1], nil
---@type string?
local rm_prefix
for i = 1, #arg do
    if arg[i] == "-o" then out_path = assert(arg[i + 1], "Error: -o requires an argument") end
    if arg[i] == "--remove-prefix" then rm_prefix = assert(arg[i + 1], "Error: --remove-prefix requires an argument") end
end

if not in_path then
    print("Usage: ffi-to-lls.lua <input header> [-o <output file>] [--remove-prefix <prefix>]")
    os.exit(1)
end

if not utilities.file_exists(in_path) then
    error("File not found: "..in_path)
end

local out_f = out_path and assert(io.open(out_path, "w")) or io.stdout

--match anything before the last dot
local mod_name = in_path:match("(.+)%..+"):gsub("/", "_"):gsub("-", "_")

---@class c.pointer<T> : { [0] : T }, ffi.ctype*

out_f:write "---@meta\n\n"
out_f:write(string.format("---@class %s\n", mod_name))
out_f:write(string.format("local %s = {}\n\n", mod_name))

local index = clang.createIndex(0, 0)
local tu = clang.parseTranslationUnit(index, in_path, nil, 0, nil, 0, clang.CXTranslationUnit_None)

if not tu then
    error("Failed to parse translation unit")
end

---@type { [string] : integer }
local cxtype_cache = {}

---@param param string
---@return integer
local function cxtype(param)
    if cxtype_cache[param] then return cxtype_cache[param] end
    local i = tonumber(clang["CXType_"..param])
    if not i then
        error("Unknown type: "..param)
    end

    cxtype_cache[param] = i
    return i
end
---@type { [integer] : string }
local LLS_TYPES = {
    [cxtype "Void"] = "nil",
    [cxtype "Bool"] = "boolean",
    [cxtype "Char_U"] = "integer",
    [cxtype "UChar"] = "integer",
    [cxtype "Char16"] = "integer",
    [cxtype "Char32"] = "integer",
    [cxtype "UShort"] = "integer",
    [cxtype "UInt"] = "integer",
    [cxtype "ULong"] = "integer",
    [cxtype "ULongLong"] = "integer",
    [cxtype "UInt128"] = "integer",
    [cxtype "Char_S"] = "integer",
    [cxtype "SChar"] = "integer",
    [cxtype "WChar"] = "integer",
    [cxtype "Short"] = "integer",
    [cxtype "Int"] = "integer",
    [cxtype "Long"] = "integer",
    [cxtype "LongLong"] = "integer",
    [cxtype "Int128"] = "integer",
    [cxtype "Float"] = "number",
    [cxtype "Double"] = "number",
    [cxtype "LongDouble"] = "number",
    [cxtype "NullPtr"] = "nil",
}

---@param x { [0] : ffi.cdata* }
---@return ffi.cdata*
local function deref(x) return x[0] end

local functype_to_lls
---@param type CXType
---@return string
local function type_to_lls_type(type)
    ---@type string?
    local lls_type = LLS_TYPES[tonumber(type.kind)]
    if lls_type then return lls_type end

    if type.kind == clang.CXType_Pointer then
        local is_const = clang.isConstQualifiedType(type)
        ---@type CXType
        local backing_type = clang.getPointeeType(type)
        if      backing_type.kind == cxtype "FunctionProto"         then return functype_to_lls(backing_type)
        elseif  backing_type.kind == cxtype "Char_S" and is_const   then return "string"
        elseif  backing_type.kind == cxtype "Void"                  then return "ffi.cdata*" end

        return string.format("c.pointer<%s>", type_to_lls_type(backing_type))
    elseif type.kind == cxtype "Elaborated" then
        return type_to_lls_type(clang.Type_getNamedType(type))
    elseif type.kind == cxtype "Record" then
        return clang.getCursorSpelling(clang.getTypeDeclaration(type))
    elseif type.kind == cxtype "Typedef" then
        return clang.getTypeSpelling(type)
    elseif type.kind == cxtype "ConstantArray" then
        local backing_type = clang.getArrayElementType(type)
        return string.format("%s[]", type_to_lls_type(backing_type))
    elseif type.kind == cxtype "Enum" then
        return clang.getCursorSpelling(clang.getTypeDeclaration(type))
    end

    warn("Unknown type: "..clang.getTypeSpelling(type).." (kind: "..clang.getTypeKindSpelling(type.kind).." - `"..tostring(type.kind).."`)")
    return clang.getTypeSpelling(type)
end

---@param type CXType
---@return string
functype_to_lls = function(type)
    local ret_type = type_to_lls_type(clang.getResultType(type))
    local params = {}

    for i = 0, clang.getNumArgTypes(type) - 1 do
        -- table.insert(params, type_to_lls_type(clang.getArgType(type, i)))
        params[i + 1] = "arg_"..i..": "..type_to_lls_type(clang.getArgType(type, i))
    end

    return string.format("fun(%s): %s", table.concat(params, ", "), ret_type)
end


---@param cursor CXCursor
local function field(cursor)
    return out_f:write(string.format("---@field %s %s\n", clang.getCursorSpelling(cursor), type_to_lls_type(clang.getCursorType(cursor))))
end

---@param param string
---@return integer
local function cursortype(param) return assert(tonumber(clang["CXCursor_"..param])) end
local visitor = {}

visitor[cursortype "StructDecl"] = function (cursor, parent)
    local name = clang.getCursorSpelling(cursor)
    out_f:write("---@class "..name.."\n")

    utilities.c.visit_cursor(cursor, function (cursor_ptr, parent_ptr)
        local cursor = deref(cursor_ptr) --[[@as CXCursor]]
        if clang.getCursorKind(cursor) == clang.CXCursor_FieldDecl then
            field(cursor)
        end

        return clang.CXChildVisit_Continue
    end)

    out_f:write("\n")
end

visitor[cursortype "EnumDecl"] = function (cursor, parent)
    local name = clang.getCursorSpelling(cursor)
    out_f:write("---@enum "..name.."\n")
    out_f:write(string.format("local %s = {\n", name))

    ---@type string[]
    local enum_names = {}
    utilities.c.visit_cursor(cursor, function (cursor_ptr, parent_ptr)
        local cursor = deref(cursor_ptr) --[[@as CXCursor]]
        if clang.getCursorKind(cursor) == clang.CXCursor_EnumConstantDecl then
            local name = clang.getCursorSpelling(cursor)
            local value = assert(tonumber(clang.getEnumConstantDeclValue(cursor)))
            out_f:write(string.format("    %s = %d,\n", name, value))
            enum_names[#enum_names+1] = name
        end

        return clang.CXChildVisit_Continue
    end)

    out_f:write("}\n\n")

    --now add them to the module table so there is proper autocompletion
    for i = 1, #enum_names do
        out_f:write(string.format("%s.%s = %s.%s\n", mod_name, enum_names[i], name, enum_names[i]))
    end

    out_f:write("\n")
end

local KW_REPLACE_MAP = {
    ["end"] = "_end",
    ["function"] = "_function",
    ["local"] = "_local",
    ["return"] = "_return",
    ["nil"] = "_nil",
    ["true"] = "_true",
    ["false"] = "_false",
    ["and"] = "_and",
    ["or"] = "_or",
    ["not"] = "_not",
    ["in"] = "_in",
    ["if"] = "_if",
    ["then"] = "_then",
    ["else"] = "_else",
    ["elseif"] = "_elseif",
    ["while"] = "_while",
    ["do"] = "_do",
    ["for"] = "_for",
    ["repeat"] = "_repeat",
    ["until"] = "_until",
    ["break"] = "_break",
    ["goto"] = "_goto",
}
setmetatable(KW_REPLACE_MAP, { __index = function(_, k) return k end })

--[[
format:
```lua
---@param param1 paramtype1
---@param param2 paramtype2
---@return returntype
function modname.funcname(param1, param2) end
]]
visitor[cursortype "FunctionDecl"] = function (cursor, parent)
    local name = clang.getCursorSpelling(cursor)
    local ret_type = clang.getCursorResultType(cursor)
    local varadict = clang.isFunctionTypeVariadic(clang.getCursorType(cursor)) == 1
    ---@type { name: string, type: string }[]
    local params = {}
    for i = 0, clang.getNumArgTypes(clang.getCursorType(cursor)) - 1 do
        ---@type CXCursor
        local arg = clang.Cursor_getArgument(cursor, i)
        local name = KW_REPLACE_MAP[tostring(clang.getCursorSpelling(arg))]
        if name == nil or name == "" or #name == 0 then name = "arg_"..i end
        local type = clang.getCursorType(arg)
        params[i + 1] = { name = name, type = type_to_lls_type(type) }
    end

    if varadict then
        params[#params + 1] = { name = "...", type = "any" }
    end

    for param in pairs(params) do
        out_f:write(string.format("---@param %s %s\n", params[param].name, params[param].type))
    end

    out_f:write(string.format("---@return %s\n", type_to_lls_type(ret_type)))
    out_f:write(string.format("function %s.%s(", mod_name, name))
    for i = 1, #params do
        out_f:write(params[i].name..(i == #params and "" or ", "))
    end

    out_f:write(") end\n")

    if rm_prefix then
        out_f:write(string.format("%s.%s = %s.%s\n", mod_name, tostring(name):gsub(rm_prefix, ""), mod_name, name))
    end

    out_f:write("\n")
end

visitor[cursortype "TypedefDecl"] = function (cursor, parent)
    local name = clang.getCursorSpelling(cursor)
    local type = type_to_lls_type(clang.getTypedefDeclUnderlyingType(cursor))
    if not name == type then out_f:write(string.format("---@alias %s %s\n", name, type)) end
end

local cursor = clang.getTranslationUnitCursor(tu)
utilities.c.visit_cursor(cursor, function (cursor_ptr, parent_ptr)
    local cursor, parent = deref(cursor_ptr), deref(parent_ptr)
    local kind = assert(tonumber(clang.getCursorKind(cursor)))

    if visitor[kind] then
        visitor[kind](cursor, parent)
    else
        warn("Unknown cursor kind: "..clang.getCursorKindSpelling(kind))
    end

    return clang.CXChildVisit_Continue
end)

out_f:write(string.format("return %s\n", mod_name))
