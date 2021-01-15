#!/usr/bin/env python

import sys
import socket


BOXES = {
    "dlukes.ucnk": "54:BF:64:7C:0B:A1",
}


def send_magic_packet(mac, family, addr):
    type = socket.SOCK_DGRAM
    sock = socket.socket(family, type)
    # Is this some kind of TTL thing? The first option is for IPv4 and
    # has a default of 0, the second one is for IPv6 and has a default
    # of 1 (it controls forwarding beyond a single subnetwork). You may
    # want to tweak them if you get errors, your magic packets keep
    # getting lost etc.
    if len(addr) == 2:
        sock.setsockopt(socket.SOL_SOCKET, socket.SO_BROADCAST, 1)
    # else:
    #     sock.setsockopt(socket.IPPROTO_IPV6, socket.IPV6_MULTICAST_HOPS, 5)
    sock.connect(addr)

    # `12 * "f"` pads the synchronization stream
    packet = bytes.fromhex(12 * "f" + 16 * mac.replace(":", ""))
    sock.sendall(packet)
    sock.close()


def main():
    _, box = sys.argv
    candidates = [k for k in BOXES if k.startswith(box)]
    num_candidates = len(candidates)

    if num_candidates == 0:
        raise ValueError(f"Unknown WoL target: {box}")

    elif num_candidates == 1:
        mac = BOXES[candidates[0]]
        port = 9

        # IPv6
        #
        # There may be multiple network interfaces (cf. `ip link`), you
        # don't know which one of these your target box is reachable on,
        # better broadcast to all of them except loopback.
        for scope_id, if_name in socket.if_nameindex()[1:]:
            print(
                f"Multicasting WoL magic packet with IPv6 to all link-local nodes on interface {if_name}, targeting MAC address {mac}."
            )
            send_magic_packet(mac, socket.AF_INET6, ("ff02::1", port, 0, scope_id))

        # IPv4
        #
        # With IPv4, there's no canonical way to specify which interface
        # should be used, so if there are multiple possibilities, it's
        # basically down to chance. See also:
        #
        # - unlike IPv6 which has scope ID, IPv4 doesn't worry about
        #   address ambiguity: https://stackoverflow.com/q/48328995/1826241
        # - workarounds for specifying an interface under IPv4 which
        #   however require sudo: https://stackoverflow.com/q/8437726/1826241
        print(f"Broadcasting WoL magic packet with IPv4, targeting MAC address {mac}.")
        send_magic_packet(mac, socket.AF_INET, ("<broadcast>", port))

    else:
        raise ValueError(f"{box!r} matches multiple candidates: {candidates}")


if __name__ == "__main__":
    main()
