minimize
obj: 0.06488999999999999 p_buy_1_
subject to
Confort_low_1,1_: -1 x_1,1_ <= -2
Confort_low_2,1_: -1 x_3,1_ <= -2
Confort_low_3,1_: -1 x_5,1_ <= 0
Confort_low_4,1_: -1 x_7,1_ <= -2
Confort_low_5,1_: -1 x_9,1_ <= 40
Confort_hig_1,1_: 1 x_1,1_ <= 8
Confort_hig_2,1_: 1 x_3,1_ <= 7
Confort_hig_3,1_: 1 x_5,1_ <= 9
Confort_hig_4,1_: 1 x_7,1_ <= 7
Confort_hig_5,1_: 1 x_9,1_ <= -18
Big_M_freezer1_1_: 1 x_9,1_ - 1 sp_5,1_ - 100000 __freezer_1_ <= 1
Big_M_freezer2_1_: -1 x_9,1_ + 1 sp_5,1_ + 100000 __freezer_1_ <= 100001
Big_M_fridge_1_: 1 u_req_1,1_ + 1 u_req_2,1_ + 1 u_req_3,1_ + 1 u_req_4,1_ - 100000 __fridge_1_ <= 3300
Compressor_req_bound1_1_: -1 u_req_1,1_ - 1 u_req_2,1_ - 1 u_req_3,1_ - 1 u_req_4,1_ + 1 p1_1_ <= 0
Compressor_req_bound2_1,1_: 1 u_1,1_ - 1 u_req_1,1_ <= 0
Compressor_req_bound2_2,1_: 1 u_2,1_ - 1 u_req_2,1_ <= 0
Compressor_req_bound2_3,1_: 1 u_3,1_ - 1 u_req_3,1_ <= 0
Compressor_req_bound2_4,1_: 1 u_4,1_ - 1 u_req_4,1_ <= 0
PV_balance_1_: 1 PVused_1_ + 1 PVcurt_1_ <= 0
Power_sell_1_: 1 p_sell_1_ + 1 PVused_1_ <= 0
Gain_1,1_: -2500 x_1,1_ + 2500 sp_1,1_ + 1 u_req_1,1_ >= 0
Gain_2,1_: -2500 x_3,1_ + 2500 sp_2,1_ + 1 u_req_2,1_ >= 0
Gain_3,1_: -2500 x_5,1_ + 2500 sp_3,1_ + 1 u_req_3,1_ >= 0
Gain_4,1_: -2500 x_7,1_ + 2500 sp_4,1_ + 1 u_req_4,1_ >= 0
Dynamics: 1 x_1,1_ + 0.0018681696582445468 u_1,1_ = 3.508634168014179
Dynamics_1: 1 x_2,1_ + 0.0002840796513771582 u_1,1_ = 3.077344483404015
Dynamics_2: 1 x_3,1_ + 0.0018075052249088852 u_2,1_ = 4.412444421787007
Dynamics_3: 1 x_4,1_ + 0.0003471953262189788 u_2,1_ = 4.079224543086318
Dynamics_4: 1 x_5,1_ + 0.0019461908997653273 u_3,1_ = 2.7582352079913406
Dynamics_5: 1 x_6,1_ + 0.00027056421269797674 u_3,1_ = 2.1054117106984735
Dynamics_6: 1 x_7,1_ + 0.0018293455754157585 u_4,1_ = 5.515533402846551
Dynamics_7: 1 x_8,1_ + 0.00031207105144341455 u_4,1_ = 5.087945685737349
Dynamics_8: 1 x_9,1_ + 4.47794070045528 __freezer_1_ = -17.071827971500305
Dynamics_9: 1 x_10,1_ + 1.2210322127987172 __freezer_1_ = -17.746908674851
Power_alloc_fridge_1_: 1 u_1,1_ + 1 u_2,1_ + 1 u_3,1_ + 1 u_4,1_ - 1 p1_1_ - 2900 __fridge_1_ = 0
Power_balance_1_: -1 p1_1_ - 2900 __fridge_1_ - 2700 __freezer_1_ + 1 p_buy_1_ + 1 PVused_1_ = 0
Setpoints_limits_1,1_: 2 <= 1 sp_1,1_ <= 8
Setpoints_limits_2,1_: 2 <= 1 sp_2,1_ <= 7
Setpoints_limits_3,1_: 2 <= 1 sp_3,1_ <= 5
Setpoints_limits_4,1_: 2 <= 1 sp_4,1_ <= 7
Setpoints_limits_5,1_: -40 <= 1 sp_5,1_ <= -18
Bounds
x_1,1_ free
x_2,1_ free
x_3,1_ free
x_4,1_ free
x_5,1_ free
x_6,1_ free
x_7,1_ free
x_8,1_ free
x_9,1_ free
x_10,1_ free
sp_1,1_ free
sp_2,1_ free
sp_3,1_ free
sp_4,1_ free
sp_5,1_ free
u_1,1_ >= 0
u_2,1_ >= 0
u_3,1_ >= 0
u_4,1_ >= 0
u_req_1,1_ >= 0
u_req_2,1_ >= 0
u_req_3,1_ >= 0
u_req_4,1_ >= 0
0 <= p1_1_ <= 3300
0 <= p_buy_1_ <= 1000000
0 <= p_sell_1_ <= 1000000
PVused_1_ >= 0
PVcurt_1_ >= 0
Binary
__fridge_1_
__freezer_1_
End
