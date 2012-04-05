set -e

base="$(dirname "$0")"

bld="$base/bld"
foreign="$base/foreign"
toolchain=gcc
debug=N
makejobs=5
buildmodules=Y

while test "$1" != ''
do
	case "$1" in
	
	'-d') debug=Y;;
	'-n') buildmodules=N;;
	'-b') shift; bld="$1";;
	'-t') shift; toolchain="$1";;
	'-j') shift; test "$1" -gt 0 && makejobs="$1";;
	
	esac

	shift
done

test -d "$bld" || { echo "no build directory: $bld"; false; }

case "$toolchain" in
gcc);;
gcc-nolto);;
clang);;
*) echo "toolchain should be one of: gcc gcc-nolto clang"; false
esac

bld="$(cd "$bld" && pwd)"
fgn="$(cd "$foreign" && pwd)"
dbg="$debug"

info=($(echo "$toolchain" | awk '{split($0, a, ":"); print(a[1], a[2], a[3])}'))

echo ${info[@]}

tcn="${info[0]}"
test ! -z "${info[1]}" && ttgt="ttarget=${info[1]}"
test ! -z "${info[2]}" && tver="tversion=${info[2]}"

foreignmake () {
	echo building "$@"
	froot="$fgn/$1"
	fbld="$(dirname "$bld/F/$2")"
	shift 2
	test -d "$froot" || { echo "no foreign module: $froot"; false; }
	mkdir -p "$fbld"
	( cd "$froot/src"; make -j $makejobs -f gnu.mk \
		bld="$fbld" foreign="$fgn" \
			toolchain="$tcn" "$ttgt" "$tver" \
			debug="$dbg" "$@" )
	echo
}

if test "$buildmodules" = Y
then
	true
fi

( cd "$base/src" && make -j $makejobs -f gnu.mk \
	bld="$bld" foreign="$fgn" \
		toolchain="$tcn" "$ttgt" "$tver" \
		debug="$dbg" )
