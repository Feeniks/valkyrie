#version 330 core

uniform mat4 M;
uniform mat4 VP;

attribute vec3 vPosition;
attribute vec3 vNormal;
attribute vec2 vTexCoord;

varying vec2 frag_texcoord;

void main() 
{ 
	gl_Position = VP * M * vec4(vPosition, 1.0f); 
	frag_texcoord = vTexCoord;
}