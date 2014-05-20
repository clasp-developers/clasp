
/intrusive_ptr_/ {
    subtime = $1;
    sub("ms","",subtime);
    total_subtime += $1;
}

/ms/	{
    time = $1;
    sub("ms","",time);
    total_time = time + total_time;
}


END {
    printf("    Total time: %f\n", total_time);
    printf(" Total subtime: %f\n", total_subtime);
    printf("    Percentage: %f\n", total_subtime/(total_time)*100.0)
}
