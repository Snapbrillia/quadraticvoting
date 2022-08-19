. scripts/main.sh

khAddr=$keyHoldersAddress

register_project 1 ProjectA 10000000000 $khAddr

wait_for_new_slot

register_project 2 ProjectB 10000000000 $khAddr

wait_for_new_slot

register_project 3 ProjectC 10000000000 $khAddr

wait_for_new_slot

register_project 4 ProjectD 10000000000 $khAddr

wait_for_new_slot

register_project 5 ProjectE 10000000000 $khAddr

wait_for_new_slot

donate_from_to_with 6 $(cat $preDir/1.pkh) 10000000 $khAddr

wait_for_new_slot

donate_from_to_with 7 $(cat $preDir/2.pkh) 20000000 $khAddr

wait_for_new_slot

donate_from_to_with 8 $(cat $preDir/3.pkh) 30000000 $khAddr

wait_for_new_slot

donate_from_to_with 9 $(cat $preDir/4.pkh) 40000000 $khAddr

wait_for_new_slot

donate_from_to_with 10 $(cat $preDir/1.pkh) 50000000 $khAddr

wait_for_new_slot

donate_from_to_with 11 $(cat $preDir/2.pkh) 60000000 $khAddr

wait_for_new_slot

donate_from_to_with 12 $(cat $preDir/3.pkh) 70000000 $khAddr

wait_for_new_slot

donate_from_to_with 13 $(cat $preDir/1.pkh) 80000000 $khAddr

wait_for_new_slot

donate_from_to_with 14 $(cat $preDir/2.pkh) 90000000 $khAddr

wait_for_new_slot

donate_from_to_with 15 $(cat $preDir/1.pkh) 100000000 $khAddr
