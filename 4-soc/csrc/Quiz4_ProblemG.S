    .section .text
    .globl main
    .align  2

.set TEST_DONE_FLAG, 0x00000100
.set TEST_RESULT,    0x00000104
.set RESULT_ADDR,    0x00000108

popcount_RV32I:
    add     t0, x0, x0       # count = 0
    add     t1, a0, x0       # value = x
1:  beq     t1, x0, 2f
    addi    t0, t0, 1
    addi    t2, t1, -1
    and     t1, t1, t2
    j       1b
2:  add     a0, t0, x0
    ret

main:
    li      a0, 0x12345678
    jal     ra, popcount_RV32I   # a0 = popcount

    li      t0, RESULT_ADDR
    sw      a0, 0(t0)
    li      t0, TEST_RESULT
    li      t1, 0x0F
    sw      t1, 0(t0)
    li      t0, TEST_DONE_FLAG
    li      t1, 0xCAFEF00D
    sw      t1, 0(t0)
    ret
