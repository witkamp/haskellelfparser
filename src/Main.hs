{-
Copyright (c) 2009 Theodore Witkamp

Permission is hereby granted, free of charge, to any person
obtaining a copy of this software and associated documentation
files (the "Software"), to deal in the Software without
restriction, including without limitation the rights to use,
copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the
Software is furnished to do so, subject to the following
conditions:

The above copyright notice and this permission notice shall be
included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
OTHER DEALINGS IN THE SOFTWARE.

-}

{-  Discription
    elftool is a utility to dump information about elf files.
    ELF files are used my most operating system to represent loadable binaries.
    elftool is still in proof of concept.
-}

module Main where

import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LBSChar
import System.Environment
import System.IO
import Control.Monad (forM,forM_)
import Data.Elf
import Data.Bits
import Data.Word
import Data.Int
import Data.Binary.Get
import Data.Binary
import Numeric


-- elftool entry point
main = 
  do  args <- getArgs
      case args of
        -- read in the elf file
        [file] ->  do   bytes <- LBS.readFile $ file
                        let elf = parseElf bytes
                        putStrLn "ELF Header Values"
                        printHeader elf
                        putStrLn ""
                        -- Dump Elf Sections
                        putStrLn "ELF Sections"
                        printSec elf
                        putStrLn ""
                        -- Dump the String table for debuging
                        putStrLn "Dumping .strtab"
                        let sym_list = (dump_strtab.elf_strtab) elf
                        forM_ sym_list  $  \(o,s) ->
                            do  let gs = getElfString elf (fromIntegral o)
                                    err  = gs /= s
                                if err
                                  then putStrLn "ERROR"
                                  else do   putStr (addSpace 5.shows o $ "") 
                                            putStrLn gs

                        -- Sump the Symbol Table
                        printSym elf
        -- Print command line usage
        _ ->      do    putStrLn "usage: elftool filename"
                        putStrLn ""

printHeader :: Elf -> IO()
printHeader elf = 
  do
    forM_ hmembers $ \m -> do
      putStrLn $ m $ elf
  where 
    
    hmembers = [show.elfClass
               ,show.elfData
               ,show.elfVersion
               ,show.elfOSABI
               ,show.elfABIVersion
               ,show.elfType
               ,show.elfMachine 
               ]


-- Some nice little helpers for printing columns
addSpace n str =  
  let r = n - length str
  in addSpace' r str

addSpace' n str
        | n <= 1  = str ++ " "
        | otherwise = (addSpace' (n -1) str ) ++ " "


-- print a ELF Section
printSec :: Elf -> IO()

printSec elf = 
  do  let list = [addSpace 15.elfSectionName
                 ,addSpace 20.show.elfSectionType
                 ,addSpace 16.(\s -> (showHex.elfSectionAddr) s "")
                 ,("bytes: " ++).addSpace 5.show.elfSectionSize
                 ,addSpace 10.show.LBS.length.elfSectionData 
                 ]
      forM_ (elfSections elf) $  \s -> 
        do  putStrLn $ foldr (\x y-> (x s) ++ "  " ++  y) "" list
{-
                putStr $ show $elfSectionName s
                putStr "\t\t"
                putStr $ show $ elfSectionType s   
                putStr "\t\t"
                putStr $ showHex $ elfSectionAddr s  
                putStr "\t\t"
                putStrLn $ show $ elfSectionSize s  
-}                


-- Get the Symbol Table section
elfFindSection :: Elf -> String -> ElfSection

elfFindSection elf name =
  head  $ filter  (\x -> elfSectionName x == name) (elfSections elf)


-- strtab section

elf_strtab elf = 
  let strtab   = elfFindSection elf ".strtab"
  in  elfSectionData strtab


dump_strtab :: LBS.ByteString -> [(Int64,String)]
dump_strtab bs = dump_strtab' bs 0
dump_strtab' bs offset = 
  let (left,right) = LBS.break (== 0) bs
      str          = LBSChar.unpack left
      consumed     = (+ 1).(+ offset).LBS.length  $ left
  in (offset,str) : if (not.LBS.null) right 
              -- break does not eat the '\0' char so drop it
              then dump_strtab' (LBS.drop 1 right)  consumed
              else []



-- Print the SymbolTable

printSym :: Elf -> IO()
printSym elf = 
  do
    putStrLn $ "Symbol Table"
    -- Get the .symtab section
    let symtab = elfFindSection  elf ".symtab" :: ElfSection
        reader = getElfReader elf
    -- Parse the Symbol Table
    forM_ (((elf_symbols reader).elfSectionData) symtab) $ 
      \s ->
      do
        putStrLn $ getElfString elf (st_name s)
        putStrLn $ show (st_value s)
        putStrLn $ show (st_size s)
        putStrLn $ show (st_bind s)
        putStrLn $ show (st_type s)
        putStrLn $ show (st_other s)
        putStrLn $ show (st_shndx s)

data ElfSymbol = ElfSymbol
  { st_name  :: Word32
  , st_value :: Word32
  , st_size  :: Word32
  --, st_info  :: Word8       -- upper 4 bits are the BIND the lower the type
  , st_bind  :: Elf_St_Bind
  , st_type  :: Elf_St_Type
  , st_other :: Word8       -- currently holds 0
  , st_shndx :: Word16      -- Index of the section referenced in the symbol
  }

{-
  Binding value for the symbol
-}
data Elf_St_Bind  = STB_LOCAL     -- value 0  Local Symbols
                  | STB_GLOBAL    -- value 1  Global Symbols
                  | STB_WEAK      -- value 2  
                  | STB_LOPROC    -- value 13 
                  | STB_MIDPROC   -- value 14 
                  | STB_HIPROC    -- value 15
                  deriving(Eq,Show)

data Elf_St_Type = STT_NOTYPE   -- 0
                 | STT_OBJECT   -- 1
                 | STT_FUNC     -- 2
                 | STT_SECTION  -- 3
                 | STT_FILE     -- 4
                 | STT_LOPROC   -- 13
                 | STT_MIDPROC  -- 14
                 | STT_HIPROC   -- 15
                 deriving(Eq,Show)

readElfSymbol er = 
  do  name  <- getWord32 er
      value <- getWord32 er
      size  <- getWord32 er
      info  <- getWord8
      other <- get
      shndx <- getWord16 er
      let _bind = _bind_read $ shiftR info 4 .&. 0xF
          _type = _type_read $ info .&. 0xF
      return $ ElfSymbol name value size _bind _type other shndx
      where _bind_read 0 = STB_LOCAL
            _bind_read 1 = STB_GLOBAL
            _bind_read 2 = STB_WEAK
            _bind_read 13 = STB_LOPROC
            _bind_read 14 = STB_MIDPROC
            _bind_read 15 = STB_HIPROC
            _type_read 0  = STT_NOTYPE
            _type_read 1  = STT_OBJECT
            _type_read 2  = STT_FUNC
            _type_read 3  = STT_SECTION
            _type_read 4  = STT_FILE
            _type_read 13 = STT_LOPROC
            _type_read 14 = STT_MIDPROC
            _type_read 15 = STT_HIPROC
            

-- Parse the Symbol Table
elf_symbols:: ElfReader -> LBS.ByteString -> [ElfSymbol]

elf_symbols er bs
  | not.LBS.null $  bs = 
        let (sym,new_bs,bytes) = runGetState (readElfSymbol er) bs 0
        in sym : (elf_symbols er new_bs)
  | otherwise = []


getElfString :: Elf -> Word32 -> String
getElfString elf offset =
  let secData = elf_strtab elf
      s       = LBS.drop (fromIntegral offset) secData
      bs      = LBS.takeWhile (\x -> x /= 0)  s
  in LBSChar.unpack bs


 