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

module Functions =
    let HardFail msg            = failwith msg
    let HardFail_InvalidCase () = HardFail "FSharp.Client.Formlet.WindowsForms.ProgrammingError: This case shouldn't be reached"

    let CopyChildren (c : Control) : ResizeArray<Control> =
        let controls    = c.Controls
        let ra          = ResizeArray<Control> (controls.Count)
        for i in 0..(controls.Count - 1) do
            ra.[i] <- controls.[i]
        ra

    let UpdateChildren (c : Control) (ra : ResizeArray<Control>) =
        let controls    = c.Controls
        controls.Clear()
        for i in 0..(ra.Count - 1) do
            controls.Add (ra.[i])

    let ActionAsDelegate (action : unit -> unit) =
        let a = Action action
        let d : Delegate = upcast a
        d

    let Dispatch (control : Control) (action : unit -> unit) =
        let d = ActionAsDelegate action
        ignore <| control.BeginInvoke (d)

    type SingleDispatchQueue<'DispatchEnum when 'DispatchEnum : enum<int32> and 'DispatchEnum : equality> (control : Control) =
        let mutable isDispatching   = false
        let queue                   = Queue<'DispatchEnum*(unit->unit)> ()

        member this.Dispatch (dispatchEnum : 'DispatchEnum, action : unit->unit) =
            let isAlreadyDispatching = queue |> Seq.exists (fun (de,_) -> de = dispatchEnum)
            if not isAlreadyDispatching then
                queue.Enqueue(dispatchEnum, action)
                this.StartDispatchIfNecessary ()

        member private this.StartDispatchIfNecessary () =
            if not isDispatching && queue.Count > 0 then
                isDispatching <- true
                let _,action = queue.Peek ()
                Dispatch control <| fun () ->
                    try
                        action ()
                    finally
                        ignore <| queue.Dequeue ()
                        isDispatching <- false
                        this.StartDispatchIfNecessary ()

    let inline CreateControl<'Control when 'Control :> Control> (c : obj) (creator : unit -> 'Control)=
        match c with
        | :? 'Control as c  -> c
        | _                 -> creator ()

