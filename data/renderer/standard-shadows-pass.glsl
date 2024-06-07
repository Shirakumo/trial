#section FRAGMENT_SHADER
#include (trial::trial "cube-maps.glsl")
uniform sampler2DArray shadow_map;

vec2 shadow_texel_size;

vec2 poisson_disk[16] = vec2[](
   vec2( -0.94201624, -0.39906216 ),
   vec2( 0.94558609, -0.76890725 ),
   vec2( -0.094184101, -0.92938870 ),
   vec2( 0.34495938, 0.29387760 ),
   vec2( -0.91588581, 0.45771432 ),
   vec2( -0.81544232, -0.87912464 ),
   vec2( -0.38277543, 0.27676845 ),
   vec2( 0.97484398, 0.75648379 ),
   vec2( 0.44323325, -0.97511554 ),
   vec2( 0.53742981, -0.47373420 ),
   vec2( -0.26496911, -0.41893023 ),
   vec2( 0.79197514, 0.19090188 ),
   vec2( -0.24188840, 0.99706507 ),
   vec2( -0.81409955, 0.91437590 ),
   vec2( 0.19984126, 0.78641367 ),
   vec2( 0.14383161, -0.14100790 )
);

float random(vec4 seed4){
  float dot_product = dot(seed4, vec4(12.9898,78.233,45.164,94.673));
  return fract(sin(dot_product) * 43758.5453);
}

float shadow_factor(ShadowMapInfo info, vec3 uvw, float map, float bias){
  float closest = texture(shadow_map, vec3(uvw.xy, map)).r;
  float current = uvw.z;
  float shadow = 0;
  for(int i=0; i<info.sample_count; ++i){
    int index = int(16*random(vec4(gl_FragCoord.xyy, i)))%16;
    vec2 poisson = poisson_disk[index]*info.sample_spread;
    for(int x=-1; x<=1; ++x){
      for(int y=-1; y<=1; ++y){
        vec2 pos = uvw.xy + poisson + vec2(x, y)*shadow_texel_size;
        float closest = texture(shadow_map, vec3(pos, map)).r;
        if((current - bias) > closest)
          shadow+=(1.0 / (info.sample_count*8.0));
      }
    }
  }
  return clamp(shadow, 0.0, 1.0);
}

float shadow_bias(vec3 normal, vec3 light_direction){
  return clamp(0.005*tan(acos(dot(normal, light_direction))), 0.0, 0.001);
}

void standard_init@after(){
  shadow_texel_size = 0.5 / textureSize(shadow_map, 0).xy;
}

StandardLightData evaluate_light_point(in StandardLight light){
  StandardLightData data = call_next_method();
  if(light.shadow_map < 0xFFFF){
      ShadowMapInfo info = shadow_info[light.shadow_map];
      int shadow_map = light.shadow_map + cubemap_texture_index(normalize(world_position-light.position));
      vec4 light_space_position = shadow_info[shadow_map].projection_matrix * vec4(world_position, 1);
      vec3 projected = (light_space_position.xyz / light_space_position.w + 1) * 0.5;
      float bias = shadow_bias(normal, data.direction);
      data.radiance *= 1.0-shadow_factor(info, projected, shadow_map, bias);
  }
  return data;
}

StandardLightData evaluate_light_directional(in StandardLight light){
  StandardLightData data = call_next_method();
  if(light.shadow_map < 0xFFFF){
      ShadowMapInfo info = shadow_info[light.shadow_map];
      vec4 light_space_position = info.projection_matrix * vec4(world_position, 1);
      vec3 projected = (light_space_position.xyz / light_space_position.w + 1) * 0.5;
      float bias = shadow_bias(normal, data.direction);
      data.radiance *= 1.0-shadow_factor(info, projected, light.shadow_map, bias);
  }
  return data;
}

StandardLightData evaluate_light_spot(in StandardLight light){
  StandardLightData data = call_next_method();
  if(light.shadow_map < 0xFFFF){
      ShadowMapInfo info = shadow_info[light.shadow_map];
      vec4 light_space_position = info.projection_matrix * vec4(world_position, 1);
      vec3 projected = (light_space_position.xyz / light_space_position.w + 1) * 0.5;
      float bias = shadow_bias(normal, data.direction);
      data.radiance *= 1.0-shadow_factor(info, projected, light.shadow_map, bias);
  }
  return data;
}
