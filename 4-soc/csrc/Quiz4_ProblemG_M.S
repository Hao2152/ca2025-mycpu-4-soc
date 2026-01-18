    .section .text
    .globl main
    .align  2

.set TEST_DONE_FLAG, 0x00000100
.set TEST_RESULT,    0x00000104
.set RESULT_ADDR,    0x00000108

popcount_RV32M:
    add     t0, x0, a0       # x
    srli    t1, t0, 1
    lui     t2, 0x55555
    addi    t2, t2, 0x555    # t2 = 0x55555555
    and     t1, t1, t2
    sub     t0, t0, t1
    lui     t2, 0x33333
    addi    t2, t2, 0x333    # t2 = 0x33333333
    and     t1, t0, t2
    srli    t3, t0, 2
    and     t3, t3, t2
    add     t0, t1, t3
    srli    t1, t0, 4
    add     t0, t0, t1
    lui     t2, 0x0f0f1
    addi    t2, t2, -0xf1    # t2 = 0x0f0f0f0f
    and     t0, t0, t2
    lui     t2, 0x01010
    addi    t2, t2, 0x101    # t2 = 0x01010101
    mul     t0, t0, t2
    srli    a0, t0, 24
    ret

main:
    li      a0, 0x12345678
    jal     ra, popcount_RV32M

    li      t0, RESULT_ADDR
    sw      a0, 0(t0)
    li      t0, TEST_RESULT
    li      t1, 0x0F
    sw      t1, 0(t0)
    li      t0, TEST_DONE_FLAG
    li      t1, 0xCAFEF00D
    sw      t1, 0(t0)
    ret
