module Generators.Python
  ( pythonLanguageGenerators
  ) where

import Generators.General
import Types
import LanguageFunctions
import Data.List

pythonLanguageGenerators :: LanguageGenerators
pythonLanguageGenerators = LanguageGenerators generatePythonLib generatePythonInterface [("c-interface", generatePythonCInterface)]

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

generatePythonLib :: DefFile -> String
generatePythonLib = unlines . generatePythonLib'

generatePythonLib' :: DefFile -> [String]
generatePythonLib' (DefFile _ funDefs _) =
  interfaceHeader ++
  (funDefs >>= funDefToExportC) ++
  ["        void tenkei_free(", "            uint8_t *buffer,", "            size_t buffer_len", "        );"] ++
  indent
    4
    ([ "\"\"\")"
     , "ffibuilder.set_source(\"my_plugin\", \"\")"
     , "ffibuilder.embedding_init_code(read_file(\"../libtenkei-python/ffi_wrappers.py\") + \"\\n\\n\" +"
     ] ++
     indent 31 ["\"\"\""]) ++
  [ "@ffi.def_extern()"
  , "def tenkei_free(buffer, buffer_len):"
  , "    DONT_FORGET[buffer] -= 1"
  , "    if DONT_FORGET[buffer] == 0:"
  , "        del DONT_FORGET[buffer]"
  , ""
  ] ++
  (funDefs >>= funDefToExportPython) ++
  ["\"\"\" +"] ++
  indent 35 ["read_file(\"test-library.py\"))"] ++
  ["    ffibuilder.compile(target=\"libtest-library.*\")", "", "if __name__ == '__main__':", "    main()"]

generatePythonInterface :: DefFile -> String
generatePythonInterface = unlines . generatePythonInterface'

generatePythonInterface' :: DefFile -> [String]
generatePythonInterface' (DefFile _ funDefs _) =
  [ "from my_plugin import ffi, lib"
  , "import sys"
  , "sys.path.append(\"../python/libtenkei-python\")"
  , "from ffi_wrappers import call, offer"
  , ""
  , "FUN_POINTERS=[]"
  , "callback_address = int(ffi.cast(\"uintptr_t\", lib.tenkei_callback))"
  , ""
  , "def fun_to_fun_ptr(f):"
  , "    # handle = ffi.new_handle(f)"
  , "    # FUN_POINTERS.append(handle)"
  , "    FUN_POINTERS.append(f)"
  , ""
  , "    return [callback_address, 0, len(FUN_POINTERS) - 1]"
  , ""
  , "def call_with_conversion(f, indices, arg_list):"
  , "    for i in indices:"
  , "        arg_list[i] = fun_to_fun_ptr(arg_list[i])"
  , ""
  , "    return call(f)(*arg_list)"
  , ""
  , "@ffi.def_extern()"
  , "def tenkei_callback(data, *args):"
  , "    f = FUN_POINTERS[int(ffi.cast(\"uintptr_t\", data))]"
  , "    return offer(f)(*args)"
  , ""
  ] ++ (funDefs >>= pythonFunctionWrapper)

pythonFunctionWrapper :: FunDef -> [String]
pythonFunctionWrapper (FunDef name sources _) =
  ["def " ++ functionId name ++ "(*args):", "    return call_with_conversion(lib.tenkei_" ++ functionId name ++ ", " ++ show funPtrIndices ++ ", list(args))", ""]
  where
    funPtrIndices = fmap snd $ filter (isFunPtr . snd . fst) $ zip sources [0 ..]

generatePythonCInterface :: DefFile -> String
generatePythonCInterface = unlines . generatePythonCInterface'

generatePythonCInterface' :: DefFile -> [String]
generatePythonCInterface' (DefFile _ funDefs _) =
  [ "from cffi import FFI"
  , "ffibuilder = FFI()"
  , ""
  , "ffibuilder.set_source(\"my_plugin\","
  , "   r\"\"\""
  , "    \"\"\","
  , "                      libraries=['test-library'],"
  , "                      library_dirs=['../../tenkei-build'])"
  , ""
  , "ffibuilder.cdef(\"\"\""
  , "extern \"Python\" void tenkei_callback(void *, uint8_t *, size_t, uint8_t **, size_t *);"
  ] ++
  (funDefs >>= funDefToExportC) ++ ["\"\"\")", "", "if __name__ == \"__main__\":", "    ffibuilder.compile(verbose=True)"]

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
funDefToExportPython (FunDef name sources _) =
  [ "@ffi.def_extern()"
  , "def " ++ foreignFunctionId name ++ "(*args):"
  , "    return offer_with_conversion(" ++ functionId name ++ ", [" ++ funPtrIndices ++ "])(*args)"
  , ""
  ]
  where
    funPtrIndices = intercalate "," $ fmap (show . snd) $ filter (isFunPtr . snd . fst) $ zip sources [0 ..]
