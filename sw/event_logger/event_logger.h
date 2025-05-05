#pragma once
//Generated do not edit!
#include "stdint.h"
#include "stdbool.h"
#include "stdio.h"

/****

**/


extern uint32_t GlobalLogger_INDEX_BITS;
extern uint32_t GlobalLogger_SIGNATURE;
extern uint32_t GlobalLogger_EVENT_COUNT;

typedef struct GlobalLogger_info_t {
   uint32_t ctrl;
   uint32_t captured_events;
   uint32_t checksum;
   uint32_t sysclk_lsb;
   uint32_t fifo_occupancy;
   uint32_t inactive_mask;
   uint32_t signature;
   uint32_t dropped_events;
   uint32_t event_counter[];
} GlobalLogger_info_t;

bool GlobalLogger_poll(GlobalLogger_ctx* ctx, volatile uint32_t* ip_location, uint32_t mask);