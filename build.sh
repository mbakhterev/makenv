set -e

base="$(dirname "$0")"

bld="$base/bld"
foreign="$base/foreign"
toolchain=gcc
debug=no
makejobs=5

while test "$1" != ''
do
	case "$1" in
	
	'-d') debug=yes;;
	'-b') shift; bld="$1";;
	'-t') shift; toolchain="$1";;
	'-j') shift; test "$1" -gt 0 && makejobs="$1";;
	
	esac

	shift
done

test -d "$bld" || { echo "no build directory: $bld"; false; }
test -d "$bld" || { echo "no foreign modules directory: $foreign"; false; }

case "$toolchain" in
gcc);;
gcc-nolto);;
clang);;
*) echo "toolchain should be one of: gcc gcc-nolto clang"; false
esac

bld="$(cd "$bld" && pwd)"
fgn="$(cd "$foreign" && pwd)"
tcn="$toolchain"

mkcmd="make -j $makejobs -f gnu.mk bld='$bld' foreign='$fgn' toolchain='$tcn'"

test "$debug" = yes && mkcmd+=' debug=Y'

(cd "$base/src" && eval "$mkcmd")
