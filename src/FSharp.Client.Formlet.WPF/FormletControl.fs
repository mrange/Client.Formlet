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

type FormletDispatchAction =
    | Rebuild   = 1
    | Submit    = 2
    | Reset     = 3

type FormletContext () =
    interface IFormletContext with
        member this.PushTag tag     = ()
        member this.PopTag ()       = ()

type FormletControl<'TValue> (scrollViewer : ScrollViewer, submit : 'TValue -> unit, formlet : Formlet<FormletContext, UIElement, 'TValue>) as this =
    inherit DecoratorElement (scrollViewer)

    let queue                       = SingleDispatchQueue<FormletDispatchAction> (this.Dispatcher)
    let mutable formTree            = Empty
    let mutable changeGeneration    = 0

    let onLoaded v = this.BuildForm ()

    do
        AddRoutedEventHandler FormletElement.SubmitEvent   this this.OnSubmit
        AddRoutedEventHandler FormletElement.ResetEvent    this this.OnReset

        scrollViewer.HorizontalScrollBarVisibility  <- ScrollBarVisibility.Disabled
        scrollViewer.VerticalScrollBarVisibility    <- ScrollBarVisibility.Visible

        this.Loaded.Add onLoaded

    let layout = FormletLayout.New TopToBottom Maximize Maximize

    let setElement (collection : IList<UIElement>) (position : int) (element : UIElement) : unit =
        if position < collection.Count then
            collection.[position] <- element
        else if position = collection.Count then
            ignore <| collection.Add element
        else
            HardFail_InvalidCase ()

    let getElement (collection : IList<UIElement>) (position : int) : UIElement =
        if position < collection.Count then collection.[position]
        else null

    let postProcessElements (collection : IList<UIElement>) (count : int) =
        let c = collection.Count
        for i = c - 1 downto count do
            collection.RemoveAt i

    let createLayout () = LayoutElement ()

    let rec buildTree (collection : IList<UIElement>) (position : int) (fl : FormletLayout) (ft : FormletTree<UIElement>) : int =
        let current = getElement collection position

        // TODO: Layout should be set
        match ft with
        | Empty                 ->
            setElement collection position null
            1
        | Element e           ->
            setElement collection position e
            1
        | Adorner (e, ls, ft) ->
            let c = buildTree ls 0 fl ft
            postProcessElements ls c
            setElement collection position e
            1
        | Many (e, adorners) ->
            let c = adorners.Count
            for i = 0 to c - 1 do
                let ls,ft   = adorners.[i]
                let c       = buildTree ls 0 fl ft
                postProcessElements ls c
            setElement collection position e
            1
        | Layout (l, ft)        ->
            let nl = fl.Union l
            if nl = fl then
                buildTree collection position fl ft
            else
                let lay         = CreateElement current createLayout
                lay.Orientation <- fl.Orientation

                let ls          = lay.ChildCollection
                let c           = buildTree ls 0 fl ft
                postProcessElements ls c
                setElement collection position lay
                1
        | Fork (l,r)            ->
            let lcount = buildTree collection position fl l
            let rcount = buildTree collection (position + lcount) fl r
            lcount + rcount
        | Modify (modifier, ft)     ->
            let c       = buildTree collection position fl ft
            let element = getElement collection position
            modifier element
            c   // TODO: Should map be applied to last, first or all?
        | Group (_, ft)         ->
            buildTree collection position fl ft
        | Cache (_, ft)         ->
            buildTree collection position fl ft

    let cacheInvalidator () = queue.Dispatch (FormletDispatchAction.Rebuild  , this.BuildForm)

    new (submit : 'TValue -> unit, formlet : Formlet<FormletContext, UIElement, 'TValue>) =
        let scrollViewer = ScrollViewer ()
        FormletControl (scrollViewer, submit, formlet)

    member this.OnSubmit    (sender : obj) (e : RoutedEventArgs) = queue.Dispatch (FormletDispatchAction.Submit   , this.SubmitForm)
    member this.OnReset     (sender : obj) (e : RoutedEventArgs) = queue.Dispatch (FormletDispatchAction.Reset    , this.ResetForm)

    member this.ResetForm () =
        formTree <- Empty
        this.BuildForm ()

    member this.Evaluate () =
        let context = FormletContext ()
        let c,ft    = formlet.Evaluate (context, cacheInvalidator, formTree)
        // TODO: "Dispose" visual elements that are no longer in tree
        formTree <- ft
        // TODO: Remove
        printfn "Result: %A" c
        printfn "Tree: %A" formTree
        printfn "=============================================================="
        c,ft

    member this.SubmitForm () =
        let c,_ = this.Evaluate ()

        if not c.HasFailures then
            submit c.Value

    member this.BuildForm () =
        let _,ft= this.Evaluate ()
        let cft = FormletTree.Layout (layout, ft)
        let lay = CreateElement scrollViewer.Content createLayout

        // TODO: Defer updates

        let ls  = lay.ChildCollection
        let c   = buildTree ls 0 layout cft
        postProcessElements ls c

        scrollViewer.Content <- lay

        ()
