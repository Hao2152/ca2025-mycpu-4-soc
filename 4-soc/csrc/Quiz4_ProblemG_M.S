    .section .text
    .globl main
    .align  2

.set TEST_DONE_FLAG, 0x00000100
.set TEST_RESULT,    0x00000104
.set RESULT_ADDR,    0x00000108

popcount_RV32M:
    cpop    a0, a0
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
