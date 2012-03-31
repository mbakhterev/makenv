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
tcn="$toolchain"
dbg="$debug"

foreignmake () {
	echo building "$@"
	froot="$fgn/$1"
	fbld="$(dirname "$bld/F/$2")"
	shift 2
	test -d "$froot" || { echo "no foreign module: $froot"; false; }
	mkdir -p "$fbld"
	( cd "$froot/src"; make -j $makejobs -f gnu.mk \
		bld="$fbld" foreign="$fgn" toolchain="$tcn" debug="$dbg" "$@" )
	echo
}

if test "$buildmodules" = Y
then
	true
fi

( cd "$base/src" && make -j $makejobs -f gnu.mk \
	bld="$bld" foreign="$fgn" toolchain="$tcn" debug="$dbg" )
