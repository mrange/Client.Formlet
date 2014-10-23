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

open System
open System.Windows

open FSharp.Client.Formlet.Core

open Controls

module Input =

    let Text initialText : Formlet<FormletContext, UIElement, string> =
        let eval (fc,cl,ft : FormletTree<UIElement>) =
            let e =
                match ft with
                | Element (:? InputTextElement as e)-> e
                | _                                 ->
                    new InputTextElement(initialText)
            e.ChangeNotifier <- cl
            (FormletCollect.Success e.Text), Element (e :> UIElement)

        Formlet.New eval

    let Integer v =
        let map (collect : FormletCollect<string>) : FormletCollect<int> =
            if collect.HasFailures then
                FormletCollect.New 0 collect.Failures
            else
                let mutable i = 0
                if Int32.TryParse (collect.Value, &i) then
                    FormletCollect.Success i
                else
                    FormletCollect<_>.FailWith "Input is not an integer"
        Text (v.ToString())
        |> Formlet.MapResult map
        |> Formlet.Cache

    let DateTime (initialDateTime : DateTime option) : Formlet<FormletContext, UIElement, DateTime> =
        let eval (fc,cl,ft : FormletTree<UIElement>) =
            let e =
                match ft with
                | Element (:? InputDateTimeElement as e)-> e
                | _                                 ->
                    new InputDateTimeElement(initialDateTime)
            e.ChangeNotifier <- cl

            let dt = e.DateTime
            let c =
                match dt with
                | Some d    -> FormletCollect.Success d
                | _         -> FormletCollect<_>.FailWith "Select a date"

            c, Element (e :> UIElement)

        Formlet.New eval

module Enhance =
    // TODO:
(*
    let Many (initialCount : int) (f : Formlet<FormletContext, UIElement, 'T>) : Formlet<FormletContext, UIElement, 'T[]> =
        let eval (fc,cl,ft : FormletTree<UIElement>) =
            let (me, list, ifts) =
                match ft with
                | Adorner ((:? ManyElement as me), list, fts)   ->
                    me, list, fts
                | _                         ->
                    let me  = ManyElement ()
                    let list= me.ChildCollection
                    me, upcast list, (Array.create initialCount Empty)

            me.ChangeNotifier <- cl

            let cs, nifts = ifts |> Array.map (fun ift -> f.Evaluate (fc, cl, ift)) |> Array.unzip

            let c   = Array.zeroCreate cs.Length
            let fs  = ResizeArray<_> ()
            for i in 0..cs.Length-1 do
                c.[i] <- cs.[i].Value
                fs.AddRange cs.[i].Failures

            FormletCollect.New c (fs |> Seq.toList), Adorner (me :> UIElement, list, nifts)

        Formlet.New eval
*)

    let WithLabel (l : string) (f : Formlet<FormletContext, UIElement, 'T>) : Formlet<FormletContext, UIElement, 'T> =
        let eval (fc,cl,ft : FormletTree<UIElement>) =
            let (le, list, ift) =
                match ft with
                | Adorner ((:? LabelElement as le), list, fts) when fts.Length > 0->
                    le, list, fts.[0]
                | Adorner ((:? LabelElement as le), list, _) ->
                    le, list, Empty
                | _                         ->
                    let le  = LabelElement (100.)
                    let list= le.ChildCollection
                    le, upcast list, Empty

            let c,nift = f.Evaluate (fc, cl, ift)
            le.Text <- l
            c.AddContext l, Adorner (le :> UIElement, list, [|nift|])

        Formlet.New eval


    let WithErrorVisual (f : Formlet<FormletContext, UIElement, 'T>) : Formlet<FormletContext, UIElement, 'T> =
        let eval (fc,cl,ft : FormletTree<UIElement>) =
            let ift =
                match ft with
                | Modify (_,ft)  -> ft
                | _             -> Empty
            let c,nift  = f.Evaluate (fc,cl,ift)
            let apply   = if c.HasFailures then AppendErrorAdorner else RemoveErrorAdorner
            c, Modify (apply, nift)

        Formlet.New eval

    let WithLegend (l : string) (f : Formlet<FormletContext, UIElement, 'T>) : Formlet<FormletContext, UIElement, 'T> =
        let eval (fc,cl,ft : FormletTree<UIElement>) =
            let le, list, ift =
                match ft with
                | Adorner ((:? LegendElement as le), list, fts) when fts.Length > 0->
                    le, list, fts.[0]
                | Adorner ((:? LegendElement as le), list, _) ->
                    le, list, Empty
                | _                         ->
                    let le  = LegendElement ()
                    let list= le.ChildCollection
                    le, upcast list, Empty

            let c,nift = f.Evaluate (fc, cl, ift)
            le.Text <- l
            c.AddContext l, Adorner (le :> UIElement, list, [|nift|])

        Formlet.New eval
