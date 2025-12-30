# HW2_HanoiTower_Final.S
# Final fix for register naming errors (t7 does not exist) and immediate range errors.

    .section .rodata
    .balign 1

hexchars:   .ascii "0123456789ABCDEF"
    .balign 4
pow10_tbl:
    .word 100000, 10000, 1000, 100, 10, 1

    .balign 1
obdata:     .byte 0x3c, 0x3b, 0x3a


    .section .bss
    .balign 16
stack_space:
    .skip 1024              # 1 KB stack space
stack_top:

    .section .text
    .globl _start

.set LOG_BASE, 0x80
.set LOG_STEP, 12

# ----------------------------------------------------------------------
# print_dec20
# ----------------------------------------------------------------------
print_dec20:
    addi    sp, sp, -32
    sw      ra, 28(sp)
    sw      s0, 24(sp)
    sw      s1, 20(sp)
    sw      s2, 16(sp)
    sw      s3, 12(sp)
    
    mv      s0, a0
    la      s1, pow10_tbl
    addi    s2, zero, 0
    addi    s3, zero, 6

dec20_next_div:
    beqz    s3, dec20_done
    lw      t0, 0(s1)
    addi    t1, zero, 0

dec20_sub_loop:
    bltu    s0, t0, dec20_emit_digit
    sub     s0, s0, t0
    addi    t1, t1, 1
    j       dec20_sub_loop

dec20_emit_digit:
    bnez    s2, dec20_do_print
    beqz    t1, dec20_skip_leading
    
dec20_do_print:
    addi    s2, zero, 1
    add     a0, t1, zero
    j       store_disk_num

dec20_skip_leading:
    addi    s1, s1, 4
    addi    s3, s3, -1
    j       dec20_next_div
    
dec20_done:
    beqz    s2, dec20_print0
    j       dec20_epilogue
    
dec20_print0:
    addi    a0, zero, 0
    
store_disk_num:
    # Uses t6 (move_count) passed from caller
    slli    t0, t6, 2        # t0 = t6 * 4
    add     t0, t0, t6       # t0 = t6 * 5
    slli    t0, t0, 1        # t0 = t6 * 10
    add     t0, t0, t6       # t0 = t6 * 11
    add     t0, t0, t6       # t0 = t6 * 12
    
    li      t1, LOG_BASE     # Use li for constants, it is safe now
    add     t1, t1, t0
    addi    t1, t1, 4
    sb      a0, 0(t1)
    
dec20_epilogue:
    lw      s3, 12(sp)
    lw      s2, 16(sp)
    lw      s1, 20(sp)
    lw      s0, 24(sp)
    lw      ra, 28(sp)
    addi    sp, sp, 32
    ret
    
# ----------------------------------------------------------------------
# putc_peg
#   a0: ASCII char
#   t6: Move count
#   a1: Offset (REPLACED t7 with a1)
# ----------------------------------------------------------------------
putc_peg:
    slli    t0, t6, 2
    add     t0, t0, t6
    slli    t0, t0, 1
    add     t0, t0, t6
    add     t0, t0, t6
    
    li      t1, LOG_BASE
    add     t1, t1, t0
    add     t1, t1, a1       # FIXED: Use a1 instead of t7
    sb      a0, 0(t1)
    ret


# ----------------------------------------------------------------------
# _start
# ----------------------------------------------------------------------
_start:
    li      sp, 0x100000
    addi    sp, sp, -40
    sw      ra, 36(sp)
    sw      s0, 32(sp)
    sw      s1, 28(sp)
    sw      s2, 24(sp)
    sw      s3, 20(sp)
    sw      s4, 16(sp)
    
    addi    s1, zero, 0
    addi    s2, zero, 0
    addi    s3, zero, 0
    addi    s0, zero, 1
    addi    s4, zero, 0
    
loop:
    addi    t0, zero, 8
    bge     s0, t0, finish

    mv      t4, s4             
    mv      t1, s0
    srli    t2, t1, 1          
    xor     t2, t2, t1         
    xor     t5, t2, t4         
    mv      s4, t2             
    
    addi    t6, zero, 0
    andi    t0, t5, 1
    bnez    t0, disk_found
    andi    t0, t5, 2
    bnez    t0, disk1
    andi    t0, t5, 4
    bnez    t0, disk2
    j       disk_found         

disk1:
    addi    t6, zero, 1
    j       disk_found
disk2:
    addi    t6, zero, 2
disk_found:
    
    beqz    t6, use_d0
    addi    t0, zero, 1
    beq     t6, t0, use_d1
    mv      t1, s3             
    j       got_old
use_d1:
    mv      t1, s2             
    j       got_old
use_d0:
    mv      t1, s1             
got_old:                       

    beqz    t6, small_move     
    addi    t0, zero, 3
    sub     t2, t0, t1         
    sub     t2, t2, s1         
    j       ready              
small_move:
    addi    t2, t1, 1          
    addi    t0, zero, 3
    blt     t2, t0, ready
    sub     t2, t2, t0         
ready:                         

    la      t0, obdata
    add     t3, t0, t1         
    lbu     t3, 0(t3)
    addi    t4, zero, 0x6F
    xor     t3, t3, t4         
    addi    t3, t3, -0x12      

    la      t0, obdata
    add     t4, t0, t2         
    lbu     t4, 0(t4)
    addi    t5, zero, 0x6F
    xor     t4, t4, t5         
    addi    t4, t4, -0x12      

    # --- Log the Move ---
    
    # 1. Log Disk Number
    add     t6, s0, zero       # Set t6 for store_disk_num
    addi    a0, t6, 1          
    jal     ra, print_dec20    
    
    # 2. Log Source Peg
    addi    a1, zero, 5        # FIXED: Use a1 instead of t7
    mv      a0, t3             
    jal     ra, putc_peg       
    
    # 3. Log Target Peg
    addi    a1, zero, 6        # FIXED: Use a1 instead of t7
    mv      a0, t4             
    jal     ra, putc_peg       
    
    # --- Update state ---
    beqz    t6, upd0
    addi    t0, zero, 1
    beq     t6, t0, upd1
    mv      s3, t2             
    j       next
upd1:
    mv      s2, t2             
    j       next
upd0:
    mv      s1, t2

next:
    addi    s0, s0, 1
    j       loop               

finish:
    li      t0, LOG_BASE
    li      t1, 7 * LOG_STEP
    add     t0, t0, t1
    li      t1, 0xDEADBEEF     # FIXED: Use li for 32-bit constant
    sw      t1, 0(t0)          
    
    lw      s4, 16(sp)
    lw      s3, 20(sp)
    lw      s2, 24(sp)
    lw      s1, 28(sp)
    lw      s0, 32(sp)
    lw      ra, 36(sp)
    addi    sp, sp, 40

_end_loop:
    j _end_loop