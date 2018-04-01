module Generators.Python
  ( generatePythonInterface
  ) where

import Generators.General
import Types

libHeader :: [String]
libHeader = []

interfaceHeader :: [String]
interfaceHeader =
  [ "import cffi"
  , ""
  , "def read_file(filename):"
  , "    with open(filename) as f:"
  , "        return f.read()"
  , ""
  , "def main():"
  , "    ffibuilder = cffi.FFI()"
  , "    ffibuilder.embedding_api(\"\"\""
  ]

functionId :: Identifier -> String
functionId = snakeCase

indent :: Int -> [String] -> [String]
indent = indentStr " "

foreignFunctionId :: Identifier -> String
foreignFunctionId = ("tenkei_" ++) . snakeCase

generatePythonInterface :: DefFile -> String
generatePythonInterface = unlines . generatePythonInterface'

generatePythonInterface' :: DefFile -> [String]
generatePythonInterface' (DefFile libName funDefs _) =
  interfaceHeader ++
  (funDefs >>= funDefToExportC) ++
  ["        void tenkei_free(", "            uint8_t *buffer,", "            size_t buffer_len", "        );"] ++
  indent
    4
    ([ "\"\"\")"
     , "ffibuilder.set_source(\"my_plugin\", \"\")"
     , "ffibuilder.embedding_init_code(read_file(\"ffi_wrappers.py\") + \"\\n\\n\" +"
     ] ++
     indent 31 ["\"\"\""]) ++
  [ "from collections import Counter"
  , ""
  , "DONT_FORGET=Counter()"
  , ""
  , "@ffi.def_extern()"
  , "def tenkei_free(buffer, buffer_len):"
  , "    DONT_FORGET[buffer] -= 1"
  , "    if DONT_FORGET[buffer] == 0:"
  , "        del DONT_FORGET[buffer]"
  , ""
  ] ++
  (funDefs >>= funDefToExportPython) ++
  ["\"\"\" +"] ++
  indent 35 ["read_file(\"test_library.py\"))"] ++
  ["    ffibuilder.compile(target=\"libmy_plugin.*\")", "", "if __name__ == '__main__':", "    main()"]


funDefToExportC :: FunDef -> [String]
funDefToExportC (FunDef name _ _) = indent 8
  [ "void " ++ foreignFunctionId name ++ "("
  , "    uint8_t *input,"
  , "    size_t input_len,"
  , "    uint8_t **output,"
  , "    size_t *output_len"
  , ");"
  , ""
  ]

funDefToExportPython :: FunDef -> [String]
funDefToExportPython (FunDef name _ _) =
  ["@ffi.def_extern()", "def " ++ foreignFunctionId name ++ "(*args):", "    return offer(" ++ functionId name ++ ")(*args)", ""]
