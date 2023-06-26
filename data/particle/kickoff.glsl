#section COMPUTE_SHADER

layout (local_size_x = 1, local_size_y = 1, local_size_z = 1) in;

void main(){
  uint dead = dead_count;
  uint alive_new = draw_args.x / 6;
  uint real_emit = min(dead, emit_count);
  
  emit_args = uvec3(ceil(float(real_emit) / float(EMIT_THREADS)), 1, 1);
  simulate_args = uvec3(ceil(float(alive_new+real_emit) / float(SIMULATE_THREADS)), 1, 1);
  draw_args = uvec4(0, 1, 0, 0);

  alive_count = alive_new;
  real_emit_count = real_emit;
}
