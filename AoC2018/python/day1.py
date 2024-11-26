import time

with open("inputday1.txt", 'r', encoding='utf-8') as file:
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

# the key is the mod of the partial sum
# the value is the (partial sum, index of that partial sum in data)
tracking_sums: dict[int, (int, int)] = {}
sum_part2: int = 0
pairs_of_mods: list[(int, int, int)] = []

min_difference_in_sums_pair: tuple = None
min_difference_in_sums: int = None
index_of_smaller_sums_pair:int = None
result:int = None

def get_index_of_smaller_sum(s1: tuple[int, int], s2: tuple[int, int]) -> int:
    return s1[1] if s1[0] < s2[0] else s2[1]
    
# go through each patial sum and get all the pairs of partial sums that have the same modulo of the sum of a total round of data
for i,d in enumerate(data):
    # if there is already a value in the dict for that modulo then you know there is a pair of patial sums with the same modulo
    if sum_part2 % sum_part_1 - 1 != tracking_sums.get(sum_part2 % sum_part_1, sum_part2 % sum_part_1 - 1):
        previous_partials_sum_with_same_mod: int = tracking_sums[sum_part2 % sum_part_1][0]
        if previous_partials_sum_with_same_mod == sum_part2:
            result: int = sum_part2
            break
        elif min_difference_in_sums_pair == None:
            min_difference_in_sums_pair = (sum_part2, previous_partials_sum_with_same_mod)
            min_difference_in_sums = abs(sum_part2 - previous_partials_sum_with_same_mod)
            index_of_smaller_sums_pair = get_index_of_smaller_sum((sum_part2, i), (previous_partials_sum_with_same_mod, tracking_sums[sum_part2 % sum_part_1][1]))
        elif ((abs(sum_part2 - previous_partials_sum_with_same_mod) < min_difference_in_sums) 
            or  (get_index_of_smaller_sum((sum_part2, i), (previous_partials_sum_with_same_mod, tracking_sums[sum_part2 % sum_part_1][1])) < index_of_smaller_sums_pair and min_difference_in_sums == abs(sum_part2 - previous_partials_sum_with_same_mod))):
            min_difference_in_sums_pair = (sum_part2, previous_partials_sum_with_same_mod)
            min_difference_in_sums = abs(sum_part2 - previous_partials_sum_with_same_mod)
            index_of_smaller_sums_pair = get_index_of_smaller_sum((sum_part2, i), (previous_partials_sum_with_same_mod, tracking_sums[sum_part2 % sum_part_1][1]))
            
    # add modulo of patial sum to the dict
    tracking_sums[sum_part2 % sum_part_1] = (sum_part2, i) 
    sum_part2 += int(d)
    
if result == None:
    result = max(min_difference_in_sums_pair)


print(result)

# # get the initial values for the comparison
# min_difference_in_sums_pair: tuple =pairs_of_mods[0]
# min_difference_in_sums:int = abs(min_difference_in_sums_pair[0]- min_difference_in_sums_pair[1])
# pairs_of_mods.remove(pairs_of_mods[0])

# for x,y,i in pairs_of_mods:
#     number_of_fullsums_btwn_matched: int = abs(x-y)
#     # if the the difference in the current pair is smaller than the intial one we know that this pair will be hit first. 
#     # Or if the difference is the same but the initial index for the first partial sum comes first that will be hit first
#     if number_of_fullsums_btwn_matched < min_difference_in_sums or (number_of_fullsums_btwn_matched == min_difference_in_sums and i < min_difference_in_sums_pair[2]):
#         min_difference_in_sums = number_of_fullsums_btwn_matched
#         min_difference_in_sums_pair = (x,y,i)
        
# answer = max(min_difference_in_sums_pair[0], min_difference_in_sums_pair[1])

# print(answer)

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
