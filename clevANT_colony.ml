(*todo : add malus to cul de sacs*)

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
    };;

let create_zone id owner pod0 pod1 visible plat =
{
id=id;
owner=owner;
pods = [|pod0;pod1|];
visible=visible;
plat=plat;
ker_dist=-1;
incomingPods = 0;
bonus = 0.;
bonusRefresh = false;
path = [];
};;
let nullZ = create_zone 0 0 0 0 0 0;;
let maxDist = ref 0.;;

let update_zone zone owner pod0 pod1 visible =
zone.owner <- owner;
zone.pods <- [|pod0;pod1|];
zone.visible <- visible;
bonusRefresh := false;;

let length = 10;;
let ratio = 2;;
let minB = 0.1;;

let rec add_bonus zone bonus =
    if bonus *. 2. > zone.bonus && bonus > minB then (
        zone.bonus <- zone.bonus +. bonus
        let rec cross l = match l with
        | [] -> ()
        | x :: xs -> (add_bonus x bonus /. ratio; cross xs) in cross !zoneLinkR.(x.id)
    );;

let score z_start z_end =
    let d = z_start.ker_dist + 1 in
    if z_end.ker_dist = -1 || z_end.ker_dist > d then
        begin
            z_end.ker_dist <- d;
            if d>1 then z_end.path <- z_start.path@[z_start];
            if !maxDist < float_of_int(d) then maxDist := float_of_int(d);
        end;
    if z_end.pods.(1-myid) > 0 then add_bonus z_end 1.;
    float_of_int(z_end.ker_dist) /. maxDist *. 10. +. (if z_end.owner = 1-myid then 10. else (if z_end.owner =-1
        then (if z_end.incomingPods = 0 then 100. else 10.) else 0.)) -. float_of_int(z_end.incomingPods) -.
        float_of_int(z_end.pods.(myid)) +. z_end.bonus*10

;;

(* Auto-generated code below aims at helping you parse *)
(* the standard input according to the problem statement. *)

(* playercount: the amount of players (always 2) *)
(* myid: my player ID (0 or 1) *)
(* zonecount: the amount of zones on the map *)
(* linkcount: the amount of links between all zones *)

let playercount, myid, zonecount, linkcount = Scanf.sscanf (input_line stdin) " %d  %d  %d  %d"
(fun playercount myid zonecount linkcount -> (playercount, myid, zonecount, linkcount));;
let zones = Array.make zonecount nullZ;;
let zoneLinkR = ref [||];;
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
        if !first then zones.(zid) <- create_zone zid ownerid podsp0 podsp1 visible platinum
        else update_zone zones.(zid) ownerid podsp0 podsp1 visible;
        if zones.(zid).pods.(myid) > 0 then
            begin
                podList := zones.(zid) :: !podList;
                if !first then (zones.(zid).ker_dist <- 0;
            end

    done;
    if !first then
        zoneLinkR := Array.map (fun l -> List.map (fun x -> zones.(x)) l) tempZoneLink;
    let rec crossPods l = match l with
        | [] -> ()
        | x :: xs -> begin
        for _ = 1 to x.pods.(myid) do
            let y :: ys = !zoneLinkR.(x.id) in let enemies = ref y.pods.(myid)
            and new_id = ref y.id
            and bestScore = ref (score x y) in
            let rec crossZ t = match t with
            | [] -> ()
            | z :: zs -> let sc = score(x,z) in
                    if sc > !bestScore then (bestScore:=sc; new_id:=z.id); enemies += z.pods.(1-myid); crossZ zs in
            crossZ ys;
            if (score x x) +. float_of_int(!enemies) > !bestScore then x.incomingPods <- x.incomingsPods + 1;
            else (
                x.incomingPods <- x.incomingsPods - 1;
                zones.(!new_id).incomingPods <- zones.(!new_id).incomingPods + 1;
                result := !result ^ (Printf.sprintf "1 %d %d ", x.id, !new_id)
            )

        done; crossPods xs; end in crossPods podList;
    (* Write an action using print_endline *)
    (* To debug: prerr_endline "Debug message"; *)


    (* first line for movement commands, second line no longer used (see the protocol in the statement for details) *)
    print_endline !result;
    print_endline "WAIT";
    first := false;
done;
