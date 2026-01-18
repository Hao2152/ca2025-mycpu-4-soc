    .section .text
    .globl main
    .align  2

.set TEST_DONE_FLAG, 0x00000100
.set TEST_RESULT,    0x00000104
.set RESULT_ADDR,    0x00000108

software_modulo_I:
    add     t0, x0, x0       # t0 = quotient = 0 
    add     t1, x0, x0       # t1 = remainder = 0
    addi    t2, x0, 32       # t2 = bit_count = 32

mod_I_loop:
    slli    t1, t1, 1        # remainder <<= 1
    srli    t3, a0, 31       # t3 = MSB(dividend)
    or      t1, t1, t3       # remainder |= MSB
    slli    a0, a0, 1        # dividend <<= 1

    blt     t1, a1, mod_I_skip_sub # if remainder < divisor, skip subtraction
    sub     t1, t1, a1       # remainder -= divisor
    ori     t0, t0, 1        # quotient |= 1 
mod_I_skip_sub:
    slli    t0, t0, 1        # quotient <<= 1 

    addi    t2, t2, -1       # bit_count--
    bne     t2, x0, mod_I_loop

    add     a0, t1, x0       # a0 = remainder
    jalr    x0, ra, 0        # return

main:
    li      a0, 1234
    li      a1, 64

    jal     ra, software_modulo_I   
    li      t5, RESULT_ADDR
    sw      a0, 0(t5)

    li      t4, TEST_RESULT
    li      t6, 0x0F
    sw      t6, 0(t4)

    li      t5, TEST_DONE_FLAG
    li      t6, 0xCAFEF00D
    sw      t6, 0(t5)

    ret
