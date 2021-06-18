function perf
  echo "\
Intro
=====

These are quick performance diagnostics for a Linux system -- stuff that
you run in the first minute after you start investigating an issue.
Source:

https://netflixtechblog.com/linux-performance-analysis-in-60-000-milliseconds-accc10403c55

The commentary excerpts are also adapted from there.
"
  hline =

  echo "
Uptime
======

This is a quick way to view the load averages, which indicate the number
of tasks (processes) wanting to run. On Linux systems, these numbers
include processes wanting to run on CPU, as well as processes blocked in
uninterruptible I/O (usually disk I/O). This gives a high level idea of
resource load (or demand), but can’t be properly understood without
other tools. Worth a quick look only.

The three numbers are exponentially damped moving sum averages with
a 1 minute, 5 minute, and 15 minute constant. The three numbers give us
some idea of how load is changing over time. For example, if you’ve been
asked to check a problem server, and the 1 minute value is much lower
than the 15 minute value, then you might have logged in too late and
missed the issue.

A large load means a lot of something: probably CPU demand; vmstat or
mpstat will confirm, which are commands 3 and 4 in this sequence.

Note that \"acceptable\" load values scale with the number of CPU cores.
If you have 24 logical threads, you can afford up to 24 tasks wanting to
run at the same time without experiencing problems.
"
  report uptime

  echo "
Last 10 system messages
=======================

Look for errors that can cause performance issues. E.g. OOM-killer, TCP
dropping a request.
"
  report 'dmesg | tail'

  echo "
Virtual memory stat
===================

The first line of output (in this version of vmstat) has some columns
that show the average since boot. The remaining ones are real-time
reports for each consecutive second.

Columns to check:

- r: Number of processes running on CPU and waiting for a turn. This
  provides a better signal than load averages for determining CPU
  saturation, as it does not include I/O. To interpret: an “r” value
  greater than the CPU count is saturation.
- free: Free memory in kilobytes. If there are too many digits to count,
  you have enough free memory. The “free -m” command, included as
  command 7, better explains the state of free memory.
- si, so: Swap-ins and swap-outs. If these are non-zero, you’re out of
  memory.
- us, sy, id, wa, st: These are breakdowns of CPU time, on average
  across all CPUs. They are user time, system time (kernel), idle, wait
  I/O, and stolen time (by other guests, or with Xen, the guest’s own
  isolated driver domain).

The CPU time breakdowns will confirm if the CPUs are busy, by adding
user + system time. A constant degree of wait I/O points to a disk
bottleneck; this is where the CPUs are idle, because tasks are blocked
waiting for pending disk I/O. You can treat wait I/O as another form of
CPU idle, one that gives a clue as to why they are idle.

System time is necessary for I/O processing. A high system time average, over 20%, can be interesting to explore further: perhaps the kernel is processing the I/O inefficiently.

In the above example, CPU time is almost entirely in user-level, pointing to application level usage instead. The CPUs are also well over 90% utilized on average. This isn’t necessarily a problem; check for the degree of saturation using the “r” column.
"
  report 'vmstat --wide 1 10'

  echo "
Per-CPU breakdown
=================

This command prints CPU time breakdowns per CPU, which can be used to
check for an imbalance. A single hot CPU can be evidence of
a single-threaded application.
"
  report 'mpstat -P ALL 1 5'

  echo "
Top with history
================

Pidstat is a little like top’s per-process summary, but prints a rolling
summary instead of clearing the screen. This can be useful for watching
patterns over time, and also recording what you saw (copy-n-paste) into
a record of your investigation.

The %CPU column is the total across all CPUs; a value of e.g. 1591%
shows that the corresponding process is consuming almost 16 CPUs.
"
  report 'pidstat 1 5'

  echo "
Block devices / IO
==================

This is a great tool for understanding block devices (disks), both the
workload applied and the resulting performance. Look for:

- r/s, w/s, rkB/s, wkB/s: These are the delivered reads, writes, read
  Kbytes, and write Kbytes per second to the device. Use these for
  workload characterization. A performance problem may simply be due to
  an excessive load applied.
- await: The average time for the I/O in milliseconds. This is the time
  that the application suffers, as it includes both time queued and time
  being serviced. Larger than expected average times can be an indicator
  of device saturation, or device problems.
- avgqu-sz: The average number of requests issued to the device. Values
  greater than 1 can be evidence of saturation (although devices can
  typically operate on requests in parallel, especially virtual devices
  which front multiple back-end disks.)
- %util: Device utilization. This is really a busy percent, showing the
  time each second that the device was doing work. Values greater than
  60% typically lead to poor performance (which should be seen in
  await), although it depends on the device. Values close to 100%
  usually indicate saturation.

If the storage device is a logical disk device fronting many back-end
disks, then 100% utilization may just mean that some I/O is being
processed 100% of the time, however, the back-end disks may be far from
saturated, and may be able to handle much more work.

Bear in mind that poor performing disk I/O isn’t necessarily an
application issue. Many techniques are typically used to perform I/O
asynchronously, so that the application doesn’t block and suffer the
latency directly (e.g., read-ahead for reads, and buffering for writes).
"
  report 'iostat -xz 1 5'

  echo "
Memory
======

The output of `free` changed cca 2016, but the important thing is that
Linux uses unused RAM for buffering writes and caching reads to speed up
I/O. So the amount of \"available\" memory (as opposed to \"free\"
memory) must discount buff/cache. Cf. https://www.linuxatemyram.com/.

At the same time, buffers (buffer cache) and cached (page cache)
shouldn't be near-zero in size, which can lead to higher disk I/O
(confirm using iostat), and worse performance.

It can be additionally confusing if ZFS on Linux is used, as we do for
some services, as ZFS has its own file system cache that isn’t reflected
properly by the free -m columns. It can appear that the system is low on
free memory, when that memory is in fact available for use from the ZFS
cache as needed.
"
  report 'free -m'

  echo "
Network interface throughput
============================

Use this tool to check network interface throughput: rxkB/s and txkB/s,
as a measure of workload, and also to check if any limit has been
reached. For example, eth0 receive might reach 22 Mbytes/s, which is 176
Mbits/sec (well under, say, a 1 Gbit/sec limit).

Some versions also have %ifutil for device utilization (max of both
directions for full duplex), which is something we also use Brendan’s
nicstat tool to measure. And like with nicstat, this is hard to get
right, so it might not work correctly (e.g. value of 0.00).
"
  report 'sar -n DEV 1 5'

  echo "
Key TCP metric summary
======================

This is a summarized view of some key TCP metrics. These include:

- active/s: Number of locally-initiated TCP connections per second
  (e.g., via connect()).
- passive/s: Number of remotely-initiated TCP connections per second
  (e.g., via accept()).
- retrans/s: Number of TCP retransmits per second.

The active and passive counts are often useful as a rough measure of
server load: number of new accepted connections (passive), and number of
downstream connections (active). It might help to think of active as
outbound, and passive as inbound, but this isn’t strictly true (e.g.,
consider a localhost to localhost connection).

Retransmits are a sign of a network or server issue; it may be an
unreliable network (e.g., the public Internet), or it may be due
a server being overloaded and dropping packets.
"
  report 'sar -n TCP,ETCP 1 5'

  echo "
What next?
==========

Top et al.
----------

The top command includes many of the metrics we checked earlier. It can
be handy to run it to see if anything looks wildly different from the
earlier commands, which would indicate that load is variable.

A downside to top is that it is harder to see patterns over time, which
may be more clear in tools like vmstat and pidstat, which provide
rolling output. Evidence of intermittent issues can also be lost if you
don’t pause the output quick enough (Ctrl-S to pause, Ctrl-Q to
continue), and the screen clears.

Also consider htop, pbytop as alternatives to top.

More granular tracing
---------------------

There's strace, and also eBPF tools like bcc:

https://github.com/iovisor/bcc/blob/master/docs/tutorial.md
"
end

function hline
  echo (string repeat -n 72 $argv)
end

function report
  hline -
  echo
  echo \$ $argv
  echo
  hline -
  echo
  set -l cmd (string split -m 1 -f 1 ' ' $argv)
  if not type -q $cmd
    echo "ERROR: Command `$cmd` not available."
  else
    eval $argv
  end
  echo
  hline =
end
