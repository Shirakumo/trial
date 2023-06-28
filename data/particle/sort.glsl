#section COMPUTE_SHADER
//
// Copyright (c) 2016 Advanced Micro Devices, Inc. All rights reserved.
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in
// all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
// THE SOFTWARE.
//

#define HALF_SIZE (SORT_SIZE/2)
#define ITERATIONS (HALF_SIZE > 1024 ? HALF_SIZE/1024 : 1)
#define NUM_THREADS (HALF_SIZE/ITERATIONS)

layout (local_size_x = NUM_THREADS, local_size_y = 1, local_size_z = 1) in;

uniform int elements;
shared vec2 local_storage[SORT_SIZE];

void main(){
  int global_base_index = (SV_GroupID.x * SORT_SIZE) + SV_GroupThreadID.x;
  int local_base_index = SV_GroupIndex;
  uint elements_in_thread_group = min(SORT_SIZE, elements - (SV_GroupID.x * SORT_SIZE));

  for(uint i=0; i<2*ITERATIONS; ++i){
    if(SV_GroupIndex + i*NUM_THREADS < elements_in_thread_group){
      uint load_index = global_base_index + i*NUM_THREADS;
      local_storage[local_base_index + i*NUM_THREADS] = vec2(particle_distances[load_index], float(index_buffer[load_index]));
    }
  }
  groupMemoryBarrier();
  barrier();

  for(uint merge_size = 2; merge_size <= SORT_SIZE; merge_size = merge_size*2){
    for(uint sub_size = merge_size >> 1; 0 < sub_size; sub_size = sub_size >> 1){
      for(uint i=0; i<ITERATIONS; ++i){
        int tmp_index = SV_GroupIndex + NUM_THREADS*i;
        int index_low = tmp_index & (sub_size-1);
        int index_high = 2 * (tmp_index-index_low);
        int index = index_low + index_high;

        uint candidate = sub_size == (merge_size >> 1)
          ? index_high - index_low + (2*sub_size - 1)
          : index_high + index_low + sub_size;

        if(candidate < elements_in_thread_group){
          vec2 a = local_storage[index];
          vec2 b = local_storage[candidate];
          
          if(b.x < a.x){
            local_storage[index] = b;
            local_storage[candidate] = a;
          }
        }
        groupMemoryBarrier();
        barrier();
      }
    }
  }

  for(uint i=0; i<2*ITERATIONS; ++i){
    if(SV_GroupIndex + i*NUM_THREADS < elements_in_thread_group){
      uint load_index = local_base_index + i*NUM_THREADS;
      uint store_index = global_base_index + i*NUM_THREADS;
      particle_distances[store_index] = local_storage[load_index].x;
      index_buffer[store_index] = uint(local_storage[load_index].y);
    }
  }
}
