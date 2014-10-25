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
        |> Formlet.Validate_NonEmpty "Value must not be empty"
        |> Enhance.WithErrorVisual
        |> Enhance.WithLabel lbl

    let LabeledInteger lbl n =
        Input.Integer n
        |> Enhance.WithErrorVisual
        |> Enhance.WithLabel lbl

    let LabeledDateTime lbl dt =
        Input.DateTime dt
        |> Formlet.Validate (fun dt -> if DateTime.Now < dt then Some "Select a date before today" else None)
        |> Enhance.WithErrorVisual
        |> Enhance.WithLabel lbl

    let LabeledOption lbl defaultValue options =
        Input.Option -1 options
        |> Formlet.Validate_Option defaultValue "Select an option"
        |> Enhance.WithErrorVisual
        |> Enhance.WithLabel lbl

    [<EntryPoint>]
    [<STAThread>]
    let main argv =

        let person =
            formlet {
                let! firstName  = LabeledText       "First name"    "Mårten"
                let! lastName   = LabeledText       "Last name"     "Rånge"
                let! birthDate  = LabeledDateTime   "Birth date"    None
                return firstName, lastName, birthDate
            }
            |> Enhance.WithLegend "Person"

        let address =
            formlet {
                let! street = LabeledText       "Street"    ""
                let! zip    = LabeledText       "Zip"       ""
                return street, zip
            }
            |> Enhance.WithLegend "Address info"

        let addresses =
            let address =
                formlet {
                    let! street = LabeledText       "Street"    ""
                    let! zip    = LabeledText       "Zip"       ""
                    return street, zip
                }
            address
            |> Enhance.Many 1
            |> Formlet.Validate (fun vs -> if vs.Length > 0 then None else Some "At least one address is required")
            |> Enhance.WithLegend "Addresses"
        let empty =
            formlet {
                return "", None
            }

        let sweden =
            formlet {
                let! orgNo = LabeledText "Org no" ""
                return orgNo, None
            }

        let norway =
            formlet {
                let! orgNo  = LabeledText "Org no" ""
                let! mva    = LabeledText "MVA" ""
                return orgNo, Some mva
            }

        let companyInfo =
            let options = LabeledOption "Country" empty [|"Sweden", sweden; "Norway", norway|]
            formlet {
                let! country    = options
                let! name       = LabeledText   "Name"      ""
                let! orgNo, mva = country
                return name, orgNo, mva
            }
            |> Enhance.WithLegend "Company info"

        let f =
            formlet {
                let! firstName, lastName, birthDate = person
                let! addresses                      = addresses
                let! name, orgNo, mva               = companyInfo
                return firstName, lastName, addresses, birthDate, name, orgNo, mva
            }

        let f2 =
            formlet {
                let! country= LabeledText       "Country"   ""
                let! street = LabeledText       "Street"    ""
                let! zip    =
                    if country = "SWEDEN" then
                        LabeledText "Post no" "41767"
                    elif country = "FINLAND" then
                        FormletMonad.Return "N/A"
                    else
                        LabeledText "ZIP" ""
                let! city   = LabeledText       "City"      ""

                return country, street, zip
            }

        let f3 =
            formlet {
                let! street = LabeledText       "Street"    "Test"
                let! country= LabeledText       "Country"   "SWEDEN"
                return street, country
            }

        let f4 =
            let options = LabeledOption "Country" empty [|"Sweden", sweden; "Norway", norway|]
            formlet {
                let! country    = options
                let! name       = LabeledText   "Name"      ""
                let! orgNo, mva = country
                return name, orgNo, mva
            }

        let cf = f |> Enhance.WithErrorSummary

        let window  = Window ()
        let submit v= printfn "Submit: %A" v
        window.Content <- FormletControl(submit, cf)

        ignore <| window.ShowDialog ()

        0
