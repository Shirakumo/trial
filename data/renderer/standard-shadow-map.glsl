#section VERTEX_SHADER
#include "skin-matrix.glsl"
#include "morph.glsl"
layout (location = TRIAL_V_LOCATION) in vec3 in_position;
layout (location = TRIAL_V_JOINTS) in vec4 in_joints;
layout (location = TRIAL_V_WEIGHTS) in vec4 in_weights;
uniform mat4 model_matrix;
uniform int shadow_map_id;
uniform int animation = 0;
uniform int morphed;

void main(){
  vec3 position = in_position;
  vec3 normal = vec3(0);
  vec2 uv = vec2(0);

  if(0 < (animation & 1)) morph_vertex(position, normal, uv);
  if(0 < (animation & 2)) skin_vertex(position, normal, in_joints, in_weights);
  
  vec4 world_position = model_matrix * vec4(position, 1);
  gl_Position = shadow_info[shadow_map_id].projection_matrix * world_position;
}

#section FRAGMENT_SHADER
void main(){}
