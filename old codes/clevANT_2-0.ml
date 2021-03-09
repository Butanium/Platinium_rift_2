(*todo : add malus to cul de sacs, pathfunding to enemy base *)

let playercount, myid, zonecount, linkcount = Scanf.sscanf (input_line stdin) " %d  %d  %d  %d"
(fun playercount myid zonecount linkcount -> (playercount, myid, zonecount, linkcount));;


let (+=) x y = x := !x+y;;


type zone =
    {
    id:int;
    mutable owner:int;
    mutable pods:int array;
    mutable visible:int;
    plat:int;
    mutable ker_dist:int;
    mutable incomingPods:int;
    mutable bonus:float;
    mutable bonusRefresh:bool;
    mutable path:int list;
    mutable malus: int;
    };;

let create_zone id owner pod0 pod1 visible plat =
{
id=id;
owner=owner;
pods = [|pod0;pod1|];
visible=visible;
plat=plat;
ker_dist= (-1);
incomingPods = 0;
bonus = 0.;
bonusRefresh = false;
path = [];
malus = 0;
};;
let nullZ = create_zone 0 0 0 0 0 0;;
let maxDist = ref 0.;;
let zoneLinkR = ref [||];;

let update_zone zone owner pod0 pod1 visible =
zone.incomingPods <- 0;
zone.owner <- owner;
zone.pods <- [|pod0;pod1|];
zone.visible <- visible;
zone.bonusRefresh <- false;
zone.bonus <- zone.bonus /. 2.;
if owner != myid then zone.malus <- 0;;

let length = 10;;
let ratio = 2.;;
let minB = 0.1;;

let rec add_bonus zone bonus =
    if bonus *. 2. > zone.bonus && bonus > minB then (
        zone.bonus <- zone.bonus +. bonus;
        let rec cross l = match l with
        | [] -> ()
        | x :: xs -> (add_bonus x (bonus /. ratio); cross xs) in cross !zoneLinkR.(zone.id)
    );;

let score z_start z_end podsCount=
    let d = z_start.ker_dist + 1 in
    if z_end.ker_dist = -1 || z_end.ker_dist > d then
        begin
            z_end.ker_dist <- d;
            if d>1 then z_end.path <- z_start.path@[z_start.id];
            if !maxDist < float_of_int(d) then maxDist := float_of_int(d);
        end;
    if z_end.pods.(1-myid) > 0 then (add_bonus z_end 1.; z_start.malus <- 0; z_end.malus <- 0);
    (float_of_int(z_end.ker_dist - z_end.malus)) /. !maxDist *. 10. +. (if z_end.owner = 1-myid then 10. else (if z_end.owner = -1
        then (if z_end.incomingPods = 0 then 100. else 10.) else 0.)) -. float_of_int(z_end.incomingPods
        + z_end.pods.(myid)) /. float_of_int(podsCount+1) /. !maxDist *. 10. +. z_end.bonus *. 10. +. (if z_end.ker_dist > 0 then 0. else 0. )

;;

(* Auto-generated code below aims at helping you parse *)
(* the standard input according to the problem statement. *)

(* playercount: the amount of players (always 2) *)
(* myid: my player ID (0 or 1) *)
(* zonecount: the amount of zones on the map *)
(* linkcount: the amount of links between all zones *)

let zones = Array.make zonecount nullZ;;
let enemyBaseId = ref (-1);;
let tempZoneLink = Array.make zonecount [];;
let first = ref true;;
for i = 0 to zonecount - 1 do
    (* zoneid: this zone's ID (between 0 and zoneCount-1) *)
    (* platinumsource: Because of the fog, will always be 0 *)
    let zoneid, platinumsource = Scanf.sscanf (input_line stdin) " %d  %d" (fun zoneid platinumsource -> (zoneid, platinumsource)) in
    ();
done;

for i = 0 to linkcount - 1 do
    let zone1, zone2 = Scanf.sscanf (input_line stdin) " %d  %d" (fun zone1 zone2 -> (zone1, zone2)) in
    tempZoneLink.(zone1) <- zone2::tempZoneLink.(zone1);
    tempZoneLink.(zone2) <- zone1::tempZoneLink.(zone2);
done;


(* game loop *)
while true do
    let result = ref "" and podList = ref [] in
    let myplatinum = int_of_string (input_line stdin) in (* your available Platinum *)
    for i = 0 to zonecount - 1 do
        (* zid: this zone's ID *)
        (* ownerid: the player who owns this zone (-1 otherwise) *)
        (* podsp0: player 0's PODs on this zone *)
        (* podsp1: player 1's PODs on this zone *)
        (* visible: 1 if one of your units can see this tile, else 0 *)
        (* platinum: the amount of Platinum this zone can provide (0 if hidden by fog) *)
        let zid, ownerid, podsp0, podsp1, visible, platinum = Scanf.sscanf (input_line stdin) " %d  %d  %d  %d  %d  %d" (fun zid ownerid podsp0 podsp1 visible platinum -> (zid, ownerid, podsp0, podsp1, visible, platinum)) in
        if !first then (
            zones.(zid) <- create_zone zid ownerid podsp0 podsp1 visible platinum;
            if ownerid != myid && ownerid > -1 then enemyBaseId := zid;
        )
        else update_zone zones.(zid) ownerid podsp0 podsp1 visible;
        if zones.(zid).pods.(myid) > 0 then
            begin
                podList := zones.(zid) :: !podList;
                if !first then (zones.(zid).ker_dist <- 0);
            end


    done;
    if !first then
        zoneLinkR := Array.map (fun l -> List.map (fun x -> zones.(x)) l) tempZoneLink;
    let rec crossPods l = match l with
        | [] -> ()
        | x :: xs -> begin
        let isIsolated = ref (x.malus = 0) and maxMalus = ref (-1) in
        for i = 1 to x.pods.(myid) do
            let podCount = ref (x.pods.(myid) + abs(x.incomingPods)) in
            List.iter (fun x -> podCount += (x.pods.(myid) + abs(x.incomingPods))) !zoneLinkR.(x.id);
            let y :: ys = !zoneLinkR.(x.id) in let enemies = ref y.pods.(1-myid)
            and new_id = ref y.id
            and bestScore = ref (score x y !podCount) in
            if !isIsolated && y.ker_dist > x.ker_dist &&  y.malus = 0 then isIsolated := false else (
                if y.malus > 0 then maxMalus := max y.malus !maxMalus
            );
            let rec crossZ t = match t with
            | [] -> ()
            | z :: zs -> let sc = (score x z !podCount) in (
                    if sc > !bestScore then (bestScore:=sc; new_id:=z.id); enemies += z.pods.(1-myid);

                    if !isIsolated && z.ker_dist > x.ker_dist &&  z.malus = 0 then isIsolated := false else (
                        (* prerr_endline (Printf.sprintf "id : %d, dist : %d, malus : %d" z.id z.ker_dist z.malus); *)
                        if y.malus > 0 then maxMalus := max y.malus !maxMalus
                    ););
                    crossZ zs in
            crossZ ys;
            (* prerr_endline (Printf.sprintf "id : %d, dif : %d" x.id (!enemies - (x.incomingPods + x.pods.(myid)))); *)
            let idleScore = (score x x !podCount) +. float_of_int( !enemies - (x.incomingPods + x.pods.(myid))) +. 10. +. 10. *. zones.(!new_id).bonus in
            if not (2* !enemies - (x.incomingPods + x.pods.(myid))>=0 &&
                 idleScore > !bestScore)
            then (
                (* prerr_endline (Printf.sprintf "%f < %f bonus: %f" idleScore !bestScore zones.(!new_id).bonus); *)
                x.incomingPods <- x.incomingPods - 1;
                zones.(!new_id).incomingPods <- zones.(!new_id).incomingPods + 1;
                result := !result ^ (Printf.sprintf "1 %d %d " x.id !new_id)
            )

        done;
        if !isIsolated then (
            x.malus <- if !maxMalus != -1 then !maxMalus-2 else int_of_float(!maxDist *. 2.);
            prerr_endline (Printf.sprintf "id : %d, malus :%d, dist : %d" x.id x.malus x.ker_dist);
            List.iter (fun x-> prerr_endline (Printf.sprintf "id : %d, dist : %d" x.id x.ker_dist)) !zoneLinkR.(x.id);
            prerr_endline ""
        );

        crossPods xs; end in crossPods !podList;
    (* Write an action using print_endline *)
    (* To debug: prerr_endline "Debug message"; *)


    (* first line for movement commands, second line no longer used (see the protocol in the statement for details) *)
    print_endline !result;
    print_endline "WAIT";
    first := false;
done;
