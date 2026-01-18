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

    mv      s0, a0
    beqz    s0, 9f

    mv      a0, s0
    jal     ra, isqrt32
    mv      s1, a0

    mv      a0, s1
    jal     ra, recip_q32

    lw      ra, 28(sp)
    lw      s0, 24(sp)
    lw      s1, 20(sp)
    addi    sp, sp, 32
    ret

9:  li      a0, 0
    lw      ra, 28(sp)
    lw      s0, 24(sp)
    lw      s1, 20(sp)
    addi    sp, sp, 32
    ret

isqrt32:
    addi    sp, sp, -52
    sw      ra, 48(sp)
    sw      s0, 44(sp)
    sw      s1, 40(sp)
    sw      s2, 36(sp)
    sw      s3, 32(sp) 

    mv      s0, a0            # s0 = x
    li      s1, 0             # s1 = result

    beqz    s0, .isq_done_zero    # x = 0 special case, return 0

    clz     t0, s0            # t0 = leading zeros of x
    li      t1, 31
    sub     t1, t1, t0        # t1 = highest bit index (0..31)
    andi    t1, t1, -2        
    li      s2, 1
    sll     s2, s2, t1        

    j       .isq_loop

.isq_done_zero:
    li      s2, 0             
    j       .isq_done

.isq_loop:
    beqz    s2, .isq_done
    add     t0, s1, s2
    bltu    s0, t0, .isq_skip
    sub     s0, s0, t0
    srli    s1, s1, 1
    add     s1, s1, s2
    j       .isq_next
.isq_skip:
    srli    s1, s1, 1
.isq_next:
    srli    s2, s2, 2
    j       .isq_loop

.isq_done:
    mv      a0, s1
    lw      s3, 32(sp)
    lw      s2, 36(sp)
    lw      s1, 40(sp)
    lw      s0, 44(sp)
    lw      ra, 48(sp)
    addi    sp, sp, 52
    ret

recip_q32:
    addi    sp, sp, -32
    sw      ra, 28(sp)
    sw      s0, 24(sp)
    sw      s1, 20(sp)
    sw      s2, 16(sp)
    sw      s3, 12(sp)

    mv      s0, a0
    beqz    s0, .rcp_zero

    li      s1, 0
    li      s2, 0
    li      s3, 33
.rcp_loop:
    beqz    s3, .rcp_done
    slli    s1, s1, 1
    li      t0, 33
    beq     s3, t0, 1f
    j       2f
1:  ori     s1, s1, 1
2:  slli    s2, s2, 1
    bltu    s1, s0, .rcp_next
    sub     s1, s1, s0
    bset    s2, s2, 0
.rcp_next:
    addi    s3, s3, -1
    j       .rcp_loop

.rcp_done:
    mv      a0, s2
    lw      s3, 12(sp)
    lw      s2, 16(sp)
    lw      s1, 20(sp)
    lw      s0, 24(sp)
    lw      ra, 28(sp)
    addi    sp, sp, 32
    ret

.rcp_zero:
    li      a0, 0
    lw      s3, 12(sp)
    lw      s2, 16(sp)
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
