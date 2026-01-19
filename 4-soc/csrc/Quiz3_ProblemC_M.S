    .balign 4
test_values:
    .word 9
case_count:
    .word 1

.set TEST_DONE_FLAG, 0x00000100
.set TEST_RESULT,    0x00000104
.set RESULT_ADDR,    0x00000108

    .section .text
    .globl  main

fast_rsqrt:
    addi    sp, sp, -32
    sw      ra, 28(sp)
    sw      s0, 24(sp)
    sw      s1, 20(sp)

    mv      s0, a0              # s0 = x
    beqz    s0, 9f              # x == 0 

    clz     t0, s0              # t0 = clz(x)
    li      t1, 31
    sub     t1, t1, t0          # t1 = exp = 31 - clz(x)

    srli    t2, t1, 1           # t2 = e2 = exp >> 1

    li      t3, 16
    sub     t3, t3, t2          # t3 = 16 - e2

    li      t4, 1
    sll     s1, t4, t3          # s1 = y = 1 << (16 - exp/2)
    li      t6, 2               

1:  
    mul     t0, s1, s1          # t0 = low32(y * y)
    mv      t1, t0              # t1 = y2

    # tmp64 = (uint64_t)x * y2
    mul     t2, s0, t1          # t2 = lo32(tmp64)
    mulhu   t3, s0, t1          # t3 = hi32(tmp64) (unsigned)

    # xy2 = tmp64 >> 16
    slli    t4, t3, 16          # hi32 << 16
    srli    t5, t2, 16          # lo32 >> 16
    or      t4, t4, t5          # t4 = xy2

    # c = (3<<16) - xy2
    li      t5, 3
    slli    t5, t5, 16          # t5 = 3 << 16
    sub     t5, t5, t4          # t5 = (3<<16) - xy2

    # prod = (uint64_t)y * c
    mul     t2, s1, t5          # t2 = lo32(prod)
    mulhu   t3, s1, t5          # t3 = hi32(prod)

    # y_next = prod >> 17
    slli    t4, t3, 15          # hi32 << (32 - 17) = hi32 << 15
    srli    t5, t2, 17          # lo32 >> 17
    or      s1, t4, t5          # s1 = y = y_next

    addi    t6, t6, -1
    bnez    t6, 1b              

    mv      a0, s1

    lw      s1, 20(sp)
    lw      s0, 24(sp)
    lw      ra, 28(sp)
    addi    sp, sp, 32
    ret

9:
    li      a0, 0
    lw      s1, 20(sp)
    lw      s0, 24(sp)
    lw      ra, 28(sp)
    addi    sp, sp, 32
    ret

main:
    addi    sp, sp, -48
    sw      ra, 44(sp)
    sw      s0, 40(sp)
    sw      s1, 36(sp)
    sw      s2, 32(sp)
    sw      s3, 28(sp)
    sw      s4, 24(sp)

    la      s0, test_values
    la      s1, case_count
    lw      s1, 0(s1)
    li      s2, 0

loop:
    beq     s2, s1, done
    lw      t0, 0(s0)
    mv      s3, t0

    mv      a0, s3
    jal     ra, fast_rsqrt
    mv      s4, a0

    addi    s0, s0, 4
    addi    s2, s2, 1
    j       loop

done:
    li      t0, RESULT_ADDR
    sw      s4, 0(t0)

    li      t0, TEST_RESULT
    li      t1, 0x0F
    sw      t1, 0(t0)

    li      t0, TEST_DONE_FLAG
    li      t1, 0xCAFEF00D
    sw      t1, 0(t0)

    lw      s4, 24(sp)
    lw      s3, 28(sp)
    lw      s2, 32(sp)
    lw      s1, 36(sp)
    lw      s0, 40(sp)
    lw      ra, 44(sp)
    addi    sp, sp, 48
    ret