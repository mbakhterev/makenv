set -e

cd "$(dirname "$0")"

if test -z "$1"
then
	prefix="$HOME/Gits/0xfb"
else
	prefix="$1"
fi

test -d foreign || mkdir foreign

cd foreign

function update() {
	{ test -d "$1" && ( cd "$1"; git pull ); } || git clone "$prefix/$1"
}
