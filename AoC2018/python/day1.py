import time

with open("inputday1.txt", 'r') as file:
    data: list[str] = file.readlines()


### Part 1
sum_part_1 = 0
for s in data:
    sum_part_1 = sum_part_1 + int(s)
    
# part 1   
print(sum_part_1)


### Part 2

## Way 1 (faster computationally)
# Starting time
start_time = time.time()

tracking_sums: dict[int, (int, int)] = {}
sum_part2: int = 0
pairs_of_mods: list[(int, int, int)] = []

for i in range(len(data)):
    if sum_part2 % sum_part_1 - 1 != tracking_sums.get(sum_part2 % sum_part_1, sum_part2 % sum_part_1 - 1):
        pairs_of_mods.append((sum_part2, tracking_sums[sum_part2 % sum_part_1][0], tracking_sums[sum_part2 % sum_part_1][1]))
    tracking_sums[sum_part2 % sum_part_1] = (sum_part2, i) 
    sum_part2 += int(data[i])

min_difference_in_sums_pair: tuple =pairs_of_mods[0]
min_difference_in_sums:int = abs(min_difference_in_sums_pair[0]- min_difference_in_sums_pair[1])
pairs_of_mods.remove(pairs_of_mods[0])

for x,y,i in pairs_of_mods:
    number_of_fullsums_btwn_matched: int = abs(x-y)
    if number_of_fullsums_btwn_matched < min_difference_in_sums or (number_of_fullsums_btwn_matched == min_difference_in_sums and i < min_difference_in_sums_pair[2]):
        min_difference_in_sums = number_of_fullsums_btwn_matched
        min_difference_in_sums_pair = (x,y,i)
        
answer = max(min_difference_in_sums_pair[0], min_difference_in_sums_pair[1])

print(answer)

# Ending the timer
end_time = time.time()
execution_time = end_time - start_time
print(f"Execution Time (mod way): {execution_time:.6f} seconds")


## Way 2
# Starting time
start_time = time.time()

sum_part2 = 0
tracking_sums: dict[int,int] = {}
i = 0   
while True:
    tracking_sums[sum_part2] = 1
    sum_part2 += int(data[i])
    
    i= (i +1) % len(data)
    if tracking_sums.get(sum_part2,0) == 1:
        break

print(sum_part2)

# Ending the timer
end_time = time.time()
execution_time = end_time - start_time
print(f"Execution Time (dict way): {execution_time:.6f} seconds")

