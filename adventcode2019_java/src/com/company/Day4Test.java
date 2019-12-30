package com.company;

import static org.junit.Assert.*;

public class Day4Test {

    @org.junit.Test
    public void isPassword() {
        assertTrue(!new Day4(223450).isPassword()); // false
        assertTrue(!new Day4(123789).isPassword()); // false
        assertTrue(!new Day4(111111).isPassword());  // false
        assertTrue(new Day4(122345).isPassword()); // true
        assertTrue(!new Day4(123444).isPassword()); // false
        assertTrue(new Day4(111122).isPassword()); // true
        assertTrue(new Day4(112233).isPassword()); // true
        assertTrue(!new Day4(444555).isPassword()); // false
        assertTrue(new Day4(445555).isPassword()); // true

        long count = 0;
        for(long i=402328;i<=864247;i++) {
                        if (new Day4(i).isPassword()) {
                            count++;
                        }
        }
        System.out.println(count);
    }


}