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

namespace FSharp.Client.Formlet.WindowsForms


open System
open System.Collections.Generic
open System.Windows.Forms

open FSharp.Client.Formlet.Core
open Controls
open Functions


type FormletDispatchAction =
    | Rebuild   = 1
    | Submit    = 2
    | Reset     = 3

type FormletContext () =
    interface IFormletContext with
        member this.PushTag tag = ()
        member this.PopTag ()   = ()

type FormletControl<'TValue> (submit : 'TValue -> unit, formlet : Formlet<FormletContext, Control, 'TValue>) as this =
    inherit Control()
    let queue                       = SingleDispatchQueue<FormletDispatchAction> (this)
    let mutable formTree            = Empty
    let mutable changeGeneration    = 0

    let layout = FormletLayout.New TopToBottom Maximize Maximize

    let setControl (collection : IList<Control>) (position : int) (control : Control) : unit =
        if position < collection.Count then
            collection.[position] <- control
        else if position = collection.Count then
            ignore <| collection.Add control
        else
            HardFail_InvalidCase ()

    let getControl (collection : IList<Control>) (position : int) : Control =
        if position < collection.Count then collection.[position]
        else null

    let postProcessControls (collection : IList<Control>) (count : int) =
        let c = collection.Count
        for i = c - 1 downto count do
            collection.RemoveAt i

    let createLayout () = new LayoutControl ()

    let rec buildTree (collection : IList<Control>) (position : int) (fl : FormletLayout) (ft : FormletTree<Control>) : int =
        let current = getControl collection position

        // TODO: Layout should be set
        match ft with
        | Empty ->
            setControl collection position null
            1
        | Element e ->
            setControl collection position e
            1
        | ForEach fts ->
            let         l = fts.Length
            let mutable p = position
            for i = 0 to l - 1 do
                let ft = fts.[i]
                p <- p + buildTree collection p fl ft
            p - position
        | Adorner (e, ls, ft) ->
            let c = buildTree ls 0 fl ft
            postProcessControls ls c
            setControl collection position e
            1
        | Many (e, adorners) ->
            let c = adorners.Count
            for i = 0 to c - 1 do
                let ls,ft   = adorners.[i]
                let c       = buildTree ls 0 fl ft
                postProcessControls ls c
            setControl collection position e
            1
        | Layout (l, ft) ->
            let nl = fl.Union l
            if nl = fl then
                buildTree collection position fl ft
            else
                let lay         = CreateControl current createLayout
                lay.Orientation <- fl.Orientation

                let ls          = CopyChildren lay
                let c           = buildTree ls 0 fl ft
                postProcessControls ls c
                UpdateChildren lay ls
                setControl collection position lay
                1
        | Fork (l,r) ->
            let lcount = buildTree collection position fl l
            let rcount = buildTree collection (position + lcount) fl r
            lcount + rcount
        | Modify (modifier, ft) ->
            let c       = buildTree collection position fl ft
            let element = getControl collection position
            modifier element
            c   // TODO: Should map be applied to last, first or all?
        | Group (_, ft) ->
            buildTree collection position fl ft
        | Cache (_, ft) ->
            buildTree collection position fl ft

    let cacheInvalidator () = queue.Dispatch (FormletDispatchAction.Rebuild  , this.BuildForm)

    override this.OnCreateControl () =
        base.OnCreateControl ()

        this.BuildForm ()

    override this.OnClientSizeChanged e =
        base.OnClientSizeChanged e

        let cur = this.LayoutControl
        if cur <> null then
            cur.Size <- this.ClientSize

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
        printfn "FormletTree: \n%A" formTree
        printfn "=============================================================="
        c,ft

    member this.SubmitForm () =
        let c,_ = this.Evaluate ()

        if not c.HasFailures then
            submit c.Value

    member this.BuildForm () =
        let _,ft= this.Evaluate ()
        let cft = FormletTree.Layout (layout, ft)

        let cur = this.LayoutControl

        let lay = CreateControl cur createLayout

        // TODO: Defer updates

        let ls  = CopyChildren lay
        let c   = buildTree ls 0 layout cft
        postProcessControls ls c
        UpdateChildren lay ls

        this.LayoutControl <- lay

        ()

    member this.LayoutControl
        with get ()  : LayoutControl    =
            if this.Controls.Count > 0 then this.Controls.[0] :?> LayoutControl else null
        and  set (lc : LayoutControl )  =
            this.Controls.Clear ()
            if lc <> null then
                lc.ClientSize <- lc.ClientSize
                this.Controls.Add lc

