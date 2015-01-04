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

type FlowletControl<'TValue> (grid : Grid, submit : 'TValue -> unit, flowlet : Flowlet<IFlowletContext, UIElement, 'TValue>) as this =
    inherit DecoratorElement (grid)

    let scrollViewer    = ScrollViewer()
    let previousButton  = CreateButton "_Previous"  "Click to goto previous page"   this.CanGotoPrevious    this.GotoPrevious
    let nextButton      = CreateButton "_Next"      "Click to goto next page"       this.CanGotoNext        this.GotoNext
    let stackPanel      = CreateStackPanel Orientation.Horizontal

    let onLoaded v  = this.RunFlowlet ()

    let context     = 
        {
            new IFlowletContext with
                member x.Show (f,cont) = 
                    ()
        }

    do
        stackPanel
            |> AddPanelChild previousButton
            |> AddPanelChild nextButton
            |> ignore

        grid
            |> AddGridRow_Star 1.
            |> AddGridRow_Auto
            |> AddGridChild scrollViewer 0 0
            |> AddGridChild stackPanel   0 1
            |> ignore

        AddRoutedEventHandler FormletElement.PreviousEvent  this this.OnPrevious
        AddRoutedEventHandler FormletElement.NextEvent      this this.OnNext

        scrollViewer.HorizontalScrollBarVisibility  <- ScrollBarVisibility.Disabled
        scrollViewer.VerticalScrollBarVisibility    <- ScrollBarVisibility.Visible

        this.Loaded.Add onLoaded

    new (submit : 'TValue -> unit, flowlet : Flowlet<IFlowletContext, UIElement, 'TValue>) =
        let grid = Grid ()
        FlowletControl (grid, submit, flowlet)

    member this.GotoPrevious ()     = FormletElement.RaisePrevious this
    member this.CanGotoPrevious ()  = true

    member this.GotoNext ()         = FormletElement.RaiseNext this
    member this.CanGotoNext ()      = true

    member this.OnPrevious  (sender : obj) (e : RoutedEventArgs) = 
        ()
    member this.OnNext      (sender : obj) (e : RoutedEventArgs) = 
        ()

    member this.RunFlowlet () =
        flowlet.Continuation (context, submit, fun fc -> ())
