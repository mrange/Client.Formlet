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

namespace FSharp.Client.Formlet.WPF.TestApp

open System
open System.Windows
open FSharp.Client.Formlet.Core
open FSharp.Client.Formlet.WPF

module Main = 

    let LabeledInput lbl text = Input.Text text |> Formlet.Label lbl

    [<EntryPoint>]
    [<STAThread>]
    let main argv = 

        let f = 
            formlet {
                let! firstName  = LabeledInput "First name" "Mårten"
                let! lastName   = LabeledInput "Last name"  "Rånge"
                let! country    = LabeledInput "Country"    "SWEDEN"
                let! soc =
                    if country = "SWEDEN" then
                        LabeledInput "This is sweden"   "740531"
                    else
                        LabeledInput "This is something else"   "XXX"
                return firstName, lastName, country, soc
            } 

        let window  = Window ()
        let submit v= printfn "Submit: %A" v
        window.Content <- Controls.FormletControl(submit, f)

        ignore <| window.ShowDialog ()

        0
