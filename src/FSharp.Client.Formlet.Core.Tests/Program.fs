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

#nowarn "20"

open System

open FSharp.Client.Formlet.Core

let ColorPrint (cc : ConsoleColor) (prelude : string) (msg : string) =
    let old = Console.ForegroundColor
    try
        Console.ForegroundColor <- cc
        Console.Write prelude
        Console.WriteLine msg
    finally
        Console.ForegroundColor <- old

let mutable FailureCount = 0

let Success (msg : string) = ColorPrint ConsoleColor.Green "SUCCESS : " msg
let Failure (msg : string) =
    FailureCount <- FailureCount + 1
    ColorPrint ConsoleColor.Red "FAILURE : " msg

let CheckEq (expected : 'T) (actual : 'T) =
    if expected <> actual then
        Failure <| sprintf "EQ: %A <> %A" expected actual
        false
    else
        true


let TestDefaultValueProvider () =
    CheckEq 1                   <| (DefaultValueProvider.GetDefaultValueProviders ()).Length

    CheckEq ""                  <| DefaultValueProvider.GetDefaultValue<string> ()
    CheckEq 0                   <| DefaultValueProvider.GetDefaultValue<int> ()
    CheckEq DateTime.MinValue   <| DefaultValueProvider.GetDefaultValue<DateTime> ()
    CheckEq [||]                <| DefaultValueProvider.GetDefaultValue<int[]> ()
    CheckEq [||]                <| DefaultValueProvider.GetDefaultValue<int[]> ()
    CheckEq []                  <| DefaultValueProvider.GetDefaultValue<int list> ()
    CheckEq ("",0)              <| DefaultValueProvider.GetDefaultValue<string*int> ()
    CheckEq ("",0)              <| DefaultValueProvider.GetDefaultValue<string*int> ()
    // TODO: Add a way to support creation of generic types
    //CheckEq Map.empty           <| DefaultValueProvider.GetDefaultValue<Map<int,int>> ()

    CheckEq 4                   <| (DefaultValueProvider.GetDefaultValueProviders ()).Length

    ()

let RunTests (action : unit->unit) =
    try
        action ()
    with
    | exn -> Failure <| sprintf "While running tests caught exception: %A" exn

[<EntryPoint>]
let main argv =
    RunTests <| TestDefaultValueProvider

    if FailureCount > 0  then
        Failure <| sprintf "%d failures detected during test run" FailureCount
        999
    else
        Success "No failures detected during test run"
        0
