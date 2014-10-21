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

    let LabeledText lbl text =
        Input.Text text
        |> Formlet.Validate_NonEmpty
        |> Enhance.WithErrorVisual
        |> Enhance.WithLabel lbl

    let LabeledInteger lbl n =
        Input.Integer n
        |> Enhance.WithErrorVisual
        |> Enhance.WithLabel lbl

    [<EntryPoint>]
    [<STAThread>]
    let main argv =

        let person = 
            formlet {
                let! firstName  = LabeledText    "First name" "Mårten"
                let! lastName   = LabeledText    "Last name"  "Rånge"
                let! age        = LabeledInteger "Age"        18
                return firstName, lastName, age
            } |> Enhance.WithLegend "Person"

        let f =
            formlet {
                let! firstName, lastName, age   = person
                let! country                    = LabeledText    "Country"    "SWEDEN"
                let! soc                        =
                    if country = "SWEDEN" then
                        LabeledText "This is sweden"   "740531"
                    elif country = "FINLAND" then
                        Formlet.Return "N/A"
                    else
                        LabeledText "This is something else"   "XXX"
                return firstName, lastName, country, age, soc
            }

        let window  = Window ()
        let submit v= printfn "Submit: %A" v
        window.Content <- FormletControl(submit, f)

        ignore <| window.ShowDialog ()

        0
