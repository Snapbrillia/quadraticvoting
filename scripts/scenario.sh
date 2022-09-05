. scripts/initiation.sh

generate_wallets_and_distribute

get_newest_slot
get_newest_slot

initiate_fund

. scripts/main.sh

get_newest_slot
get_newest_slot

khAddr=$keyHoldersAddress

register_project 1 $(cat $preDir/1.pkh) ProjectA 10000000000 $khAddr

wait_for_new_slot
get_newest_slot

register_project 2 $(cat $preDir/2.pkh) ProjectB 10000000000 $khAddr

wait_for_new_slot
get_newest_slot

register_project 3 $(cat $preDir/3.pkh) ProjectC 10000000000 $khAddr

wait_for_new_slot
get_newest_slot

register_project 4 $(cat $preDir/4.pkh) ProjectD 10000000000 $khAddr

wait_for_new_slot
get_newest_slot

register_project 5 $(cat $preDir/5.pkh) ProjectE 10000000000 $khAddr

wait_for_new_slot
get_newest_slot

donate_from_to_with 6 $(cat $preDir/6.pkh) $(cat $preDir/1.pkh) 10000000 $khAddr

wait_for_new_slot
get_newest_slot

donate_from_to_with 7 $(cat $preDir/7.pkh) $(cat $preDir/2.pkh) 20000000 $khAddr

wait_for_new_slot
get_newest_slot

donate_from_to_with 8 $(cat $preDir/8.pkh) $(cat $preDir/3.pkh) 30000000 $khAddr

wait_for_new_slot
get_newest_slot

donate_from_to_with 9 $(cat $preDir/9.pkh) $(cat $preDir/4.pkh) 40000000 $khAddr

wait_for_new_slot
get_newest_slot

donate_from_to_with 10 $(cat $preDir/10.pkh) $(cat $preDir/1.pkh) 50000000 $khAddr

wait_for_new_slot
get_newest_slot

donate_from_to_with 11 $(cat $preDir/11.pkh) $(cat $preDir/2.pkh) 60000000 $khAddr

wait_for_new_slot
get_newest_slot

donate_from_to_with 12 $(cat $preDir/12.pkh) $(cat $preDir/3.pkh) 70000000 $khAddr

wait_for_new_slot
get_newest_slot

donate_from_to_with 13 $(cat $preDir/13.pkh) $(cat $preDir/1.pkh) 80000000 $khAddr

wait_for_new_slot
get_newest_slot

donate_from_to_with 14 $(cat $preDir/14.pkh) $(cat $preDir/2.pkh) 90000000 $khAddr

wait_for_new_slot
get_newest_slot

donate_from_to_with 15 $(cat $preDir/15.pkh) $(cat $preDir/1.pkh) 100000000 $khAddr