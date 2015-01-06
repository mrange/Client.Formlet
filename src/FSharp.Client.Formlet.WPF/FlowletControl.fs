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

open System.Collections.Generic
open System.Windows
open System.Windows.Controls

open FSharp.Client.Formlet.Core

open Elements
open InternalElements

[<AbstractClass>]
type BaseFlowletControl (uiElement : UIElement) =
    inherit DecoratorElement (uiElement)

type FlowletControl<'TValue> (      border  : Border
                                ,   submit  : 'TValue -> unit
                                ,   cancel  : unit -> unit
                                ,   flowlet : Flowlet<'TValue>
                                ) as this =
    inherit BaseFlowletControl (border)

    let onLoaded v      = this.RunFlowlet ()

    let pages           = Stack<BaseFormletControl>()

    let context         =
        {
            new FlowletContext with
                member x.Show (cont,pageNo,f) =
                    let fc  = FormletControl.CreateNonInteractive cont cancel f :> BaseFormletControl
                    pages.Push fc
                    border.Child <- fc
        }

    do
        AddRoutedEventHandler FormletElement.NextEvent      this this.OnNext
        AddRoutedEventHandler FormletElement.PreviousEvent  this this.OnPrevious
        AddRoutedEventHandler FormletElement.SubmitEvent    this this.OnSubmit
        AddRoutedEventHandler FormletElement.ResetEvent     this this.OnReset
        AddRoutedEventHandler FormletElement.CancelEvent    this this.OnCancel

        this.Loaded.Add onLoaded

    member this.OnPrevious  (sender : obj) (e : RoutedEventArgs) = this.PreviousPage ()
    member this.OnNext      (sender : obj) (e : RoutedEventArgs) = this.NextPage ()

    member this.OnCancel    (sender : obj) (e : RoutedEventArgs) = this.CancelFlowlet ()

    member this.OnSubmit    (sender : obj) (e : RoutedEventArgs) = this.SubmitFlowlet ()
    member this.OnReset     (sender : obj) (e : RoutedEventArgs) = this.ResetPage ()

    member this.CancelFlowlet () = 
        cancel ()

    member this.SubmitFlowlet () = 
        if pages.Count > 0 then
            let fc = pages.Peek ()
            fc.SubmitForm ()

    member this.ResetPage () = 
        if pages.Count > 0 then
            let fc = pages.Peek ()
            fc.ResetForm ()

    member this.PreviousPage () =
        if pages.Count > 1 then
            ignore <| pages.Pop ()
            let fc = pages.Peek ()
            border.Child <- fc
            fc.RebuildForm ()

    member this.NextPage () =
        if pages.Count > 0 then
            let fc = pages.Peek ()
            fc.SubmitForm ()

    member this.RunFlowlet () =
        let s (v,pageNo) = submit v
        flowlet.Continuation (context, 1, s)

module FlowletControl =
    let Create  (submit  : 'TValue -> unit)
                (cancel  : unit -> unit)
                (flowlet : Flowlet<'TValue>) =

        let border = Border ()
        FlowletControl (border, submit, cancel, flowlet)
