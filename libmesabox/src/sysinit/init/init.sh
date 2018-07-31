mount -n -t proc proc /proc
mount -n -t devtmpfs devtmpfs /dev
mount -n -t sysfs sysfs /sys

mknod -m 600 /dev/console c 5 1
mknod -m 620 /dev/tty1 c 4 1
mknod -m 666 /dev/tty c 5 0
mknod -m 666 /dev/null c 1 3
mknod -m 660 /dev/kmsg c 1 11

/bin/mgetty
