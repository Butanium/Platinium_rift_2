import sys
import math


class Zone:
    def __init__(self, z_id, owner, pod0, pod1, visible, plat):
        self.id = z_id
        self.owner = owner
        self.pods = [pod0, pod1]
        self.visible = visible
        self.plat = plat
        self.ker_dist = -1
        self.incomingPods = 0
        self.bonus = 0
        self.bonus_refresh = False

    def refresh(self, new):
        self.owner = new.owner
        self.pods = new.pods
        self.visible = new.visible
        self.plat = new.plat
        self.incomingPods = 0
        self.bonus_refresh = False


def add_bonus(zone, bonus, length=10, ratio=2, minB=.1):
    zone.bonus += bonus
    if bonus / ratio >= minB:
        for i in link_dic[zone.id]:
            add_bonus(i, bonus / ratio, length - 1, ratio, minB)


def score(start, end):
    global max_dist
    end.ker_dist = min(end.ker_dist, start.ker_dist + 1) if end.ker_dist >= 0 else start.ker_dist + 1
    if max_dist < end.ker_dist:
        max_dist = end.ker_dist
    if end.pods[1 - my_id] > 0 and not end.bonus_refresh:
        end.bonus_refresh = True
        add_bonus(end, end.pods[1 - my_id])

    return (end.owner != my_id) * 10 - end.incomingPods - end.pods[my_id] + end.bonus * 10 \
           + end.pods[1 - my_id] + end.ker_dist / max_dist * 10


# Auto-generated code below aims at helping you parse
# the standard input according to the problem statement.
link_dic = {}
# player_count: the amount of players (always 2)
# my_id: my player ID (0 or 1)
# zone_count: the amount of zones on the map
# link_count: the amount of links between all zones
player_count, my_id, zone_count, link_count = [int(i) for i in input().split()]
for i in range(zone_count):
    # zone_id: this zone's ID (between 0 and zoneCount-1)
    # platinum_source: Because of the fog, will always be 0
    zone_id, platinum_source = [int(j) for j in input().split()]
for i in range(link_count):
    zone_1, zone_2 = [int(j) for j in input().split()]
    if zone_1 in link_dic.keys():
        link_dic[zone_1] += [zone_2]
    else:
        link_dic[zone_1] = [zone_2]
    if zone_2 in link_dic.keys():
        link_dic[zone_2] += [zone_1]
    else:
        link_dic[zone_2] = [zone_1]
first = True
zone_dic = {}
max_dist = 0
# game loop
while True:
    result = ""

    my_platinum = int(input())  # your available Platinum
    pod_list = []
    for i in range(zone_count):
        # z_id: this zone's ID
        # owner_id: the player who owns this zone (-1 otherwise)
        # pods_p0: player 0's PODs on this zone
        # pods_p1: player 1's PODs on this zone
        # visible: 1 if one of your units can see this tile, else 0
        # platinum: the amount of Platinum this zone can provide (0 if hidden by fog)
        z_end, owner_id, pods_p0, pods_p1, visible, platinum = [int(j) for j in input().split()]
        z = Zone(z_end, owner_id, pods_p0, pods_p1, visible, platinum)
        if first:
            zone_dic[z_end] = z
        else:
            zone_dic[z_end].refresh(z)
        if z.pods[my_id] > 0:
            pod_list.append(zone_dic[z_end])
            if first:
                zone_dic[z_end].ker_dist = 0
    if first:
        for i in link_dic:
            link_dic[i] = list(map(lambda x: zone_dic[x], link_dic[i]))
    for z in pod_list:
        for i in range(z.pods[my_id]):
            new_id = link_dic[z.id][0].id
            best_score = score(z, zone_dic[new_id])
            for z_end in link_dic[z.id][1:]:
                sc = score(z, z_end)
                if sc > best_score:
                    best_score = sc
                    new_id = z_end.id
            zone_dic[new_id].incomingPods += 1
            result += "1 {} {} ".format(z.id, new_id)

    # Write an action using print
    # To debug: print("Debug messages...", file=sys.stderr, flush=True)

    # first line for movement commands, second line no longer used (see the protocol in the statement for details)
    print(result[:-1])
    print("WAIT")
    first = False
