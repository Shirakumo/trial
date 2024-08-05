// We need to include it as a uniform, but the struct include
// would not create the correct uniform block.
// #include trial::morph-data
#define SIMULTANEOUS_MORPHS 8
layout(std140) uniform MorphData {
  int count;
  float weights[SIMULTANEOUS_MORPHS];
  int indices[SIMULTANEOUS_MORPHS];
} morph_data;

uniform sampler1DArray morph_targets;

void morph_vertex(inout vec3 position, inout vec3 normal, inout vec2 uv){
  int vid = gl_VertexID;
  for(int i=0; i<min(morph_data.count, SIMULTANEOUS_MORPHS); ++i){
    int index = morph_data.indices[i]*3;
    float weight = morph_data.weights[i];
    position += weight * texelFetch(morph_targets, ivec2(vid, index+0), 0).xyz;
    normal += weight * texelFetch(morph_targets, ivec2(vid, index+1), 0).xyz;
    uv += weight * texelFetch(morph_targets, ivec2(vid, index+2), 0).xy;
  }
}
