index = 1
for argument in $ARGV
    printf("%d:%s\n", index, argument)
    index = index + 1
end
