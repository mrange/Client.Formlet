(* Copyright 1999-2005 The Apache Software Foundation or its licensors, as
 * applicable.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *)

namespace FSharp.Client.Formlet.WPF

open FSharp.Client.Formlet.Core

open System.Windows
open System.Windows.Controls

open Controls

module Input = 

    let Text initialText : Formlet<FormContext, UIElement, string> = 
        let eval (fc,ft : FormTree<UIElement>) = 
            match ft with
            | Singleton (:? InputTextElement as e)  -> (Formlet.Success e.Text)     , ft
            | _                                     -> (Formlet.Success initialText), Singleton (upcast new InputTextElement(initialText))

        Formlet.New eval 

module Enhance = 

    let x = 3
(*
    let Many (initialCount : int) (f : Formlet<UIElement, 'T>) : Formlet<UIElement, 'T list> =  
        let eval (fc,ft : FormTree<UIElement>) = 
            match ft with
            | Container ((:? ListBox as lb), fts : FormTree<UIElement> list) -> 
                let ifts = 
                (Formlet.Success []) , ft
            | _                         -> 
                (Formlet.Success []) , Visual (upcast new ListBox ())

        Formlet.New eval 
        
*)

