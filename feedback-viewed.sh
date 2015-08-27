#/bin/bash
file="$(pwd)/feedback-viewed-$1-$2.csv"
cmd="SELECT reviewee_id, time_stamp, feedback_viewed_time_stamp"
cmd="$cmd FROM review"
cmd="$cmd WHERE assignment_id='$1' "
cmd="$cmd AND step_id='$2' "
cmd="$cmd AND reviewee_id NOT LIKE 'default%'  "
cmd="$cmd ORDER BY feedback_viewed_time_stamp ASC "
cmd="$cmd INTO OUTFILE '$file'"
cmd="$cmd FIELDS TERMINATED BY ','"
cmd="$cmd ENCLOSED BY \"'\""
cmd="$cmd LINES TERMINATED BY '\\n'"

rm $file -f
mysql -u root --database="captain_teach" --execute="$cmd"
echo "reviewee_id, time_stamp, feedback_viewed_time_stamp\n" | cat - $file > temp && mv temp $file
