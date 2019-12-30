package com.company;

import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;
import java.util.stream.Stream;

public class Main {

    public static boolean checkDouble(long num) {
        String value = num+"";
        char pre = value.charAt(0);
        for (int i=1;i<value.length();i++) {
            if (pre == value.charAt(i)) {
                return true;
            }
            pre = value.charAt(i);
        }
        return false;
    }

    public static boolean checkDouble2(long num) {
        String value = num+"";
        char pre = value.charAt(0);
        boolean result = false;
        String sameChar = pre+"";

        for (int i=1;i<value.length();i++) {
            if (pre == value.charAt(i)) {
                sameChar += value.charAt(i);
            } else {
                if (!result) {
                    result = sameChar.length() == 2;
                }
                pre = value.charAt(i);
                sameChar = value.charAt(i)+"";

            }
        }
        return result;
    }

    public static boolean increase(long num) {
        String value = num+"";
        char[] valueArray =value.toCharArray();
        Arrays.sort(valueArray);
        return value.equals(new String(valueArray));
    }

    public static boolean isPassword(long num) {
        return increase(num) && checkDouble2(num);
    }

    public static void main(String[] args) {
	// write your code here


        long count = 0;
        for(long i=402328;i<=864247;i++) {
//            if (isPassword(i)) {
//                count++;
//            }
        }
        System.out.println(count);
    }
}
