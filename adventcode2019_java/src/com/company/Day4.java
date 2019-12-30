package com.company;

import java.util.*;

public class Day4 {

    private long num;

    public Day4(long num) {
        this.num = num;
    }


    private boolean checkDouble2(long num) {
        String value = num+"";
        char pre = value.charAt(0);
        int sameCount = 0;
        Set<Integer> sameCountList = new HashSet<>();
        for (int i=1;i<value.length();i++) {
            if (pre == value.charAt(i)) {
                sameCount++;
            } else {
                sameCountList.add(sameCount);
                sameCount = 0;
                pre = value.charAt(i);
            }
        }
        sameCountList.add(sameCount);
        return sameCountList.contains(1);
    }

    private boolean increase(long num) {
        String value = num+"";
        char[] valueArray =value.toCharArray();
        Arrays.sort(valueArray);
        return value.equals(new String(valueArray));
    }

    public boolean isPassword() {
        return increase(this.num) && checkDouble2(this.num);
    }
}
