#section COMPUTE_SHADER
#extension GL_ARB_compute_shader : require
#extension GL_ARB_shader_storage_buffer_object : require

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

layout (local_size_x = 256, local_size_y = 1, local_size_z = 1) in;

uniform int elements;
uniform ivec4 job_params;

void main(){
  uvec4 tgp = uvec4(gl_WorkGroupID.x * 256, 0, elements, clamp(elements - gl_WorkGroupID.x*512, uint(0), uint(512)));
  uint local_id = tgp.x + gl_LocalInvocationID.x;
  uint index_low = local_id & (uint(job_params.x) - 1);
  uint index_high = 2 * (local_id-index_low);

  uint index = tgp.y + index_high + index_low;
  uint candidate = tgp.y + index_high + job_params.y + job_params.z*index_low;

  if(candidate < tgp.y + tgp.z){
    float a = particle_distances[index];
    float b = particle_distances[candidate];

    if(a < b){
      particle_distances[index] = b;
      particle_distances[candidate] = a;

      uint a_index = alive_particles_1[index];
      uint b_index = alive_particles_1[candidate];
      alive_particles_1[index] = b_index;
      alive_particles_1[candidate] = a_index;
    }
  }
}
