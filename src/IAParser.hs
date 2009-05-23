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

module IAParser
where

import Text.ParserCombinators.Parsec

test = parse mangledSymbol "testcase" "_Z3fooILi2EEvRAplT_Li1E_i"

mangledSymbol =   do  string "_Z"
                      p_encoding
                      return ()
p_encoding :: Parser ()
p_encoding =    try ( p_name >> p_bare_function_type) -- function name
          <|>   p_name                                -- data name
          <|>   p_special_name

p_name :: Parser ()
p_name =  p_nested_name 
      <|> p_unscoped_name 
      <|> p_unscoped_template_name
      <|> p_local_name

p_unscoped_name :: Parser ()
p_unscoped_name =   p_unqualified_name
               <|>  ((string "St") >> p_unqualified_name) -- ::std::

p_unscoped_template_name =  p_unscoped_name
                        <|> p_substitution

p_nested_name = do  char 'N'
                    (p_cv_qualifiers <|> return () ) -- optional
                    (p_prefix >> p_unqualified_name) 
                      <|> (p_template_prefix >> p_template_args)
                    char 'E'
                    return ()

p_prefix =  (>>) p_prefix           p_unqualified_name
        <|> (>>) p_template_prefix  p_template_args
        <|> p_template_param
        <|> p_substitution
        <|> return ()-- #empty 

p_template_prefix =   (>>) p_prefix p_unqualified_name
                 <|>  p_template_param
                 <|>  p_substitution

p_unqualified_name =  (p_operator_name >> return ())
                  <|> p_ctor_dtor_name
                  <|> p_source_name

p_source_name :: Parser ()
p_source_name =  do p_number 
                    p_identifier
                    return ()
                    
p_number :: Parser Integer
p_number =  do  ds <- many1 digit
                return (read ds)
p_identifier :: Parser String
p_identifier = many1 letter


p_operator_name  =  string "nw"  -- new 
                <|> string "na"  -- new[]
                <|> string "dl"  -- delete
                <|> string "da"  -- delete[]
                <|> string "ps"  -- + (unary)
                <|> string "ng"  -- - (unary)
                <|> string "ad"  -- # & (unary)
                <|> string "de"  -- * (unary)
                <|> string "co"  -- ~
                <|> string "pl"  -- +
                <|> string "mi"  -- -             
                <|> string "ml"  -- *             
                <|> string "dv"  -- /             
                <|> string "rm"  -- %             
                <|> string "an"  -- &             
                <|> string "or"  -- |             
                <|> string "eo"  -- ^             
                <|> string "aS"  -- =             
                <|> string "pL"  -- +=            
                <|> string "mI"  -- -=            
                <|> string "mL"  -- *=            
                <|> string "dV"  -- /=            
                <|> string "rM"  -- %=            
                <|> string "aN"  -- &=            
                <|> string "oR"  -- |=            
                <|> string "eO"  -- ^=            
                <|> string "ls"  -- <<            
                <|> string "rs"  -- >>            
                <|> string "lS"  -- <<=           
                <|> string "rS"  -- >>=           
                <|> string "eq"  -- ==            
                <|> string "ne"  -- !=            
                <|> string "lt"  -- <             
                <|> string "gt"  -- >             
                <|> string "le"  -- <=            
                <|> string "ge"  -- >=            
                <|> string "nt"  -- !             
                <|> string "aa"  -- &&            
                <|> string "oo"  -- ||            
                <|> string "pp"  -- ++            
                <|> string "mm"  -- --            
                <|> string "cm"  -- ,             
                <|> string "pm"  -- ->*           
                <|> string "pt"  -- ->            
                <|> string "cl"  -- ()            
                <|> string "ix"  -- []            
                <|> string "qu"  -- ?             
                <|> string "st"  -- sizeof (a type)
                <|> string "sz"  -- sizeof (an expression)
                <|> string "at"  -- alignof (a type)
                <|> string "az"  -- alignof (an expression)
                <|> (string "cv" >> p_type) -- (cast)        
                <|> (char 'v' >> digit >> p_source_name >> return "") -- vendor extended operator



p_special_name = undefined
p_call_offset = undefined
p_nv_offset = undefined
p_v_offset = undefined
p_ctor_dtor_name = undefined
p_type = undefined
p_cv_qualifiers = undefined
p_builtin_type = undefined
p_function_type = undefined
p_class_enum_type = undefined
p_array_type = undefined
p_pointer_to_member_type = undefined
p_bare_function_type = undefined

p_template_param = undefined
p_template_template_param = undefined
p_template_args = undefined
p_template_arg = undefined
p_expression = undefined
p_expr_primary = undefined

-- Scope encoding
p_local_name = undefined
p_discriminator = undefined

-- Compression
p_substitution = undefined




