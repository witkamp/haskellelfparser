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
                        putStrLn "ELF Sections"
                        printSec elf
                        let sym_list = (dumpStrTab.elfSectionData) $ elfFindSection elf ".strtab"
                        forM_ sym_list  $  \s -> 
                          do  putStrLn  s
                          
                        --printSym elf
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
               
addSpace n str =  
  let r = n - length str
  in addSpace' r str

addSpace' n str
        | n <= 1  = str ++ " "
        | otherwise = (addSpace' (n -1) str ) ++ " "


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
printSym :: Elf -> IO()
printSym elf = 
  do
    putStrLn $ "Symbol Table"
    forM_ (parseElfSymbol(elfSectionData (elfFindSection  elf ".symtab"))) $ 
      \s ->
      do
        putStrLn $ showHex (st_name s) " " ++ (getElfString elf (st_value s))

-- Get the Symbol Table section
elfFindSection :: Elf -> String -> ElfSection

elfFindSection elf name =
  head  $ filter  (\x -> elfSectionName x == name) (elfSections elf)

dumpStrTab :: LBS.ByteString -> [String]
dumpStrTab bs = dumpStrTab' bs 0

dumpStrTab' :: LBS.ByteString -> Int64 -> [String]
dumpStrTab' bs offset = 
  let (left,right) = LBS.break (== 0) bs
      str          = (addSpace 5.shows offset $ "") ++ (LBSChar.unpack left)
      consumed     = (+ 1).(+ offset).LBS.length  $ left
  in str : if (not.LBS.null) right 
              -- break does not eat the '\0' char so drop it
              then dumpStrTab' (LBS.drop 1 right)  consumed
              else []

data ElfSymbol = ElfSymbol
  { st_name  :: Word32
  , st_value :: Word32
  , st_size  :: Word32
  , st_info  :: Word8
  , st_other :: Word8
  , st_shndx :: Word16
  }
  
  
instance Binary ElfSymbol where
  put sym = do  put $ st_name   sym
                put $ st_value  sym
                put $ st_size   sym
                put $ st_info   sym
                put $ st_other  sym
                put $ st_shndx  sym
  get     = do  name  <- get
                value <- get
                size  <- get
                info  <- get
                other <- get
                shndx <- get
                return $ ElfSymbol name value size info other shndx

-- Parse the Symbol Table
parseElfSymbol:: LBS.ByteString -> [ElfSymbol]

parseElfSymbol bs = 
  let (sym,new_bs,bytes) = runGetState (get :: Get ElfSymbol) bs 0
  in sym : (parseElfSymbol new_bs)

getElfString :: Elf -> Word32 -> String
getElfString elf offset =
  let strtab  = elfFindSection elf ".strtab"
      secData = elfSectionData strtab
      s       = LBS.drop (fromIntegral offset) secData
      bs      = LBS.takeWhile (\x -> x /= 0)  s
  in LBSChar.unpack bs
  
  
  
  
  
  