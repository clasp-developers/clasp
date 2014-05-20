#|#########################################################################
# Copyright (C) 1997-2011 Ufasoft                                         #
# http://ufasoft.com   mailto:support@ufasoft.com                         #
#                                                                         #
# This program is free software; you can redistribute it and/or modify it #
# under the terms of the GNU General Public License as published by the   #
# Free Software Foundation; either version 3, or (at your option) any     #
# later version.                                                          #
#                                                                         #
# This program is distributed in the hope that it will be useful,         #
# but WITHOUT ANY WARRANTY; without even the implied warranty of          #
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the           #
# GNU General Public License for more details.                            #
#                                                                         #
# You should have received a copy of the GNU General Public License       #
# along with this program; If not, see <http://www.gnu.org/licenses/>     #
########################################################################=|#

(defmacro _error_code (code val)
  `(defconstant ,code ,val))
;!!!  `(defconstant ,code ,(+ #x8004B3B0 val)))

(defmacro _define-error-codes (&body codes)
  (let ((n 0))
    (labels ((_defconsts (r)
               (if r (cons (list 'defconstant (car r) (incf n))
                           (_defconsts (cdr r))))))
      `(progn ,@(_defconsts codes)))))

(_define-error-codes
  E_LISP_InvalidSyntax
  E_LISP_RParExpected
  E_LISP_TooFewArgs           
  E_LISP_Exit                 
  E_LISP_SymbolExpected       
  E_LISP_SymbolNotDeclared    
  E_LISP_LabelNotFound        
  E_LISP_BadFunction          
  E_LISP_BadArgumentType      
  E_LISP_MisplacedCloseParen  
  E_LISP_InvalidDottedPair    
  E_LISP_IllegalSetfPlace     
  E_LISP_IllegalTypeSpecifier 
  E_LISP_CERROR               
  E_LISP_MaxLenOfToken        
  E_LISP_UnexpectedEOF        
  E_LISP_VariableUnbound      
  E_LISP_Return               
  E_LISP_Stop                 
  E_LISP_Run                  
  E_LISP_CallDoesNotMatch     
  E_LISP_AbsentCloseParen     
  E_LISP_MustBeSValue         
  E_LISP_InvalidLambdaList    
  E_LISP_UnboundSlot          
  E_LISP_Unwind               
  E_LISP_NoValue              
  E_LISP_NoFillPointer        
  E_LISP_IsNotVector          
  E_LISP_IsNotArray           
  E_LISP_IsNotStream          
  E_LISP_IsNegative           
  E_LISP_IsNotReadTable       
  E_LISP_IsNotHashTable       
  E_LISP_UnknownError         
  E_LISP_IsNotRandomState     
  E_LISP_NoPackageWithName    
  E_LISP_DisabledReadEval
  E_LISP_IsNotRational
  E_LISP_InvalidSeqType
  E_LISP_IllegalTestArgument
  E_LISP_NotCoercedToChar
  E_LISP_UnrecognizedCharName
  E_LISP_StackOverflow
  E_LISP_DivisionByZero
)
