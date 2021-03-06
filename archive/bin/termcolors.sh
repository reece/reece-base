#! /bin/bash
# print colored text examples to stdout
# also try with 4, 7, or '4;7' args

cw=10

echo "termcolors -- color samples and codes for color terminals"
cc="0;40;31m"
echo "e.g.,  printf '\033[$cc%s\033[0m\n'" '"Some text"'
printf "generates \033[$cc%s\033[0m\n" "Some text"


printf "%${cw}s" 'bg\fg'
fc=30;
while [ $fc -le 38 ]; do
	printf " %${cw}d" $fc
	fc=$(( $fc + 1 ))
done
echo


i=0
while [ $i -le 1 ]; do
	printf "%${cw}s" "$i;"
	fc=30;
	while [ $fc -le 38 ]; do
		cc="$i;${fc}m"
		printf " \\033[$cc%${cw}s\\033[0m" "$cc"
		fc=$(( $fc + 1 ))
	done
	echo
	i=$(( $i + 1 ))
done


bc=40
while [ $bc -le 48 ]; do
i=0
while [ $i -le 1 ]; do
	printf "%${cw}s" "$i;$bc;"
	fc=30;
	while [ $fc -le 38 ]; do
		cc="$i;$bc;${fc}m"
		printf " \\033[$cc%${cw}s\\033[0m" "$cc"
		fc=$(( $fc + 1 ))
	done
	echo
	i=$(( $i + 1 ))
done
	bc=$(( $bc + 1 ))
done


