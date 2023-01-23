#lang racket/base

(define 
  (square x)
  (* x x)
)

(define (smallest-divisor n) (find-divisor n 2))
(define 
  (find-divisor n test-divisor)
  (cond 
    ((> (square test-divisor) n) n)
    ((divides? test-divisor n) test-divisor)
    (else (find-divisor n (+ test-divisor 1)))
  )
)
(define (divides? a b) (= (remainder b a) 0))

(define 
  (prime? n)
  (= n (smallest-divisor n))
)

(define 
  (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (current-process-milliseconds))
)
(define 
  (start-prime-test n start-time)
  (if (prime? n) 
    (report-prime (- (current-process-milliseconds) start-time))
    null
  )
)
(define 
  (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time)
)

(define 
  (search-for-primes start end)
  (if (> start end) 
    null
    (if (= (remainder start 2) 1) 
      (begin 
        (timed-prime-test start)
        (search-for-primes (+ start 2) end)
      )
      (search-for-primes (+ start 1) end)
    )
  )
)

(print "1000,10000,100000より大きな素数をそれぞれ3つ見つける")
(search-for-primes 1000 1019)
(search-for-primes 10000 10037)
(search-for-primes 100000 100043)

; 上記を以下のPCで実行したところ，実行時間がどれも0ミリ秒以下であったため，比較することができなかった
; https://www.lenovo.com/jp/ja/p/laptops/thinkpad/thinkpad-e-series/e14/22tpe14e4n1

; H/W path             Device          Class          Description
; ===============================================================
;                                      system         20RAS1L000 (LENOVO_MT_20RA_BU_SMB_FM_ThinkPad E14)
; /0                                   bus            20RAS1L000
; /0/2                                 memory         8GiB System Memory
; /0/2/0                               memory         8GiB SODIMM DDR4 Synchronous 2667 MHz (0.4 ns)
; /0/b                                 memory         256KiB L1 cache
; /0/c                                 memory         1MiB L2 cache
; /0/d                                 memory         6MiB L3 cache
; /0/e                                 processor      Intel(R) Core(TM) i5-10210U CPU @ 1.60GHz
; /0/10                                memory         128KiB BIOS
; /0/100                               bridge         Intel Corporation
; /0/100/2                             display        UHD Graphics
; /0/100/4                             generic        Xeon E3-1200 v5/E3-1500 v5/6th Gen Core Processor Thermal Subsystem
; /0/100/8                             generic        Xeon E3-1200 v5/v6 / E3-1500 v5 / 6th/7th/8th Gen Core Processor Gaussian Mixture Model
; /0/100/12                            generic        Comet Lake Thermal Subsytem
; /0/100/14                            bus            Intel Corporation
; /0/100/14/0          usb1            bus            xHCI Host Controller
; /0/100/14/0/5                        input          ELECOM BlueLED Mouse
; /0/100/14/0/8                        multimedia     Integrated Camera
; /0/100/14/0/9                        generic        Goodix FingerPrint Device
; /0/100/14/0/a                        communication  Bluetooth wireless interface
; /0/100/14/1          usb2            bus            xHCI Host Controller
; /0/100/14.2                          memory         RAM memory
; /0/100/14.3          wlp0s20f3       network        Wireless-AC 9462
; /0/100/16                            communication  Comet Lake Management Engine Interface
; /0/100/17                            storage        Comet Lake SATA AHCI Controller
; /0/100/1d                            bridge         Intel Corporation
; /0/100/1d/0          enp4s0          network        RTL8111/8168/8411 PCI Express Gigabit Ethernet Controller
; /0/100/1d.4                          bridge         Intel Corporation
; /0/100/1d.4/0                        storage        KIOXIA Corporation
; /0/100/1d.4/0/0      /dev/nvme0      storage        KBG40ZNT256G TOSHIBA MEMORY
; /0/100/1d.4/0/0/1    /dev/nvme0n1    disk           256GB NVMe namespace
; /0/100/1d.4/0/0/1/1  /dev/nvme0n1p1  volume         259MiB Windows FAT volume
; /0/100/1d.4/0/0/1/2  /dev/nvme0n1p2  volume         15MiB reserved partition
; /0/100/1d.4/0/0/1/3  /dev/nvme0n1p3  volume         117GiB Windows NTFS volume
; /0/100/1d.4/0/0/1/4  /dev/nvme0n1p4  volume         999MiB Windows NTFS volume
; /0/100/1d.4/0/0/1/5  /dev/nvme0n1p5  volume         119GiB EXT4 volume
; /0/100/1f                            bridge         Intel Corporation
; /0/100/1f.3                          multimedia     Intel Corporation
; /0/100/1f.4                          bus            Intel Corporation
; /0/100/1f.5                          bus            Comet Lake SPI (flash) Controller
; /0/0                                 system         PnP device PNP0c02
; /0/1                                 system         PnP device PNP0c02
; /0/3                                 system         PnP device PNP0c02
; /0/4                                 system         PnP device PNP0c02
; /0/5                                 system         PnP device PNP0b00
; /0/6                                 generic        PnP device INT3f0d
; /0/7                                 generic        PnP device LEN0071
; /0/8                                 generic        PnP device LEN213e
; /0/9                                 system         PnP device PNP0c02
; /0/a                                 system         PnP device PNP0c02
; /0/f                                 system         PnP device PNP0c02
; /0/11                                system         PnP device PNP0c01
; /1                                   power          5B10W138

; したがって，求める素数の桁数を増やして実行時間の比較を行う
(search-for-primes 100000000000000 100000000000097)
; 100000000000031 *** 131
; 100000000000067 *** 130
; 100000000000097 *** 133
(search-for-primes 1000000000000000 1000000000000159)
; 1000000000000037 *** 417
; 1000000000000091 *** 405
; 1000000000000159 *** 405
(search-for-primes 10000000000000000 10000000000000079)
; 10000000000000061 *** 1281
; 10000000000000069 *** 1279
; 10000000000000079 *** 1283
(search-for-primes 100000000000000000 100000000000000019)
; 100000000000000003 *** 4272
; 100000000000000013 *** 4279
; 100000000000000019 *** 4275


; 桁を1つ増やすと実行時間がおおよそsqrt(10)倍になり，2つ増やすとsqrt(100) = 10倍，3つだとsqrt(1000)倍になっており，予想通りである．
