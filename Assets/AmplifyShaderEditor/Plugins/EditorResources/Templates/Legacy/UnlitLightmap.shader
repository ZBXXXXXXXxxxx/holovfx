Shader /*ase_name*/ "Hidden/Templates/Legacy/UnlitLightmap" /*end*/
{
	Properties
	{
		/*ase_props*/
	}
	
	SubShader
	{
		Tags { "RenderType"="Opaque" }
		LOD 100
		/*ase_all_modules*/
		CGINCLUDE
			#ifndef UNITY_SETUP_STEREO_EYE_INDEX_POST_VERTEX
			#define UNITY_SETUP_STEREO_EYE_INDEX_POST_VERTEX(input)
			#endif
		ENDCG
		Pass
		{
			/*ase_main_pass*/
			Tags{ "LightMode" = "VertexLMRGBM" "RenderType" = "Opaque" }
			Name "Unlit LM"
			CGPROGRAM
			#pragma target 2.0
			#pragma vertex vert
			#pragma fragment frag
			#include "UnityCG.cginc"
			/*ase_pragma*/

			struct MeshData
			{
				float4 vertex : POSITION;
				UNITY_VERTEX_INPUT_INSTANCE_ID
				/*ase_vdata:p=p*/
			};
			
			struct V2FData
			{
				float4 vertex : SV_POSITION;
				UNITY_VERTEX_INPUT_INSTANCE_ID
				UNITY_VERTEX_OUTPUT_STEREO
				/*ase_interp(0,):sp=sp.xyzw*/
			};

			/*ase_globals*/
			
			V2FData vert ( MeshData v /*ase_vert_input*/)
			{
				V2FData o;
				UNITY_SETUP_INSTANCE_ID(v);
				UNITY_INITIALIZE_VERTEX_OUTPUT_STEREO(o);
				UNITY_TRANSFER_INSTANCE_ID(v, o);
				/*ase_vert_code:v=appdata;o=v2f*/
				
				v.vertex.xyz += /*ase_vert_out:Local Vertex;Float3;_Vertex*/ float3(0,0,0) /*end*/;
				o.vertex = UnityObjectToClipPos(v.vertex);
				return o;
			}
			
			fixed4 frag (V2FData i /*ase_frag_input*/) : SV_Target
			{
				UNITY_SETUP_INSTANCE_ID( i );
				UNITY_SETUP_STEREO_EYE_INDEX_POST_VERTEX( i );
				fixed4 finalColor;
				/*ase_frag_code:i=v2f*/
				
				finalColor = /*ase_frag_out:Frag Color;Float4;_Color*/fixed4(1,1,1,1)/*end*/;
				return finalColor;
			}
			ENDCG
		}
		
		Pass
		{
			/*ase_hide_pass*/
			Tags{ "LightMode" = "VertexLM" "RenderType" = "Opaque" }
			Name "Unlit LM Mobile"
			CGPROGRAM
			#pragma target 2.0
			#pragma vertex vert
			#pragma fragment frag
			#include "UnityCG.cginc"
			/*ase_pragma*/

			struct MeshData
			{
				float4 vertex : POSITION;
				UNITY_VERTEX_INPUT_INSTANCE_ID
				/*ase_vdata:p=p*/
			};
			
			struct V2FData
			{
				float4 vertex : SV_POSITION;
				UNITY_VERTEX_OUTPUT_STEREO
				/*ase_interp(0,):sp=sp.xyzw*/
			};

			/*ase_globals*/
			
			V2FData vert ( MeshData v /*ase_vert_input*/)
			{
				V2FData o;
				UNITY_SETUP_INSTANCE_ID(v);
				UNITY_INITIALIZE_VERTEX_OUTPUT_STEREO(o);
				/*ase_vert_code:v=appdata;o=v2f*/
				
				v.vertex.xyz += /*ase_vert_out:Local Vertex;Float3;_Vertex*/ float3(0,0,0) /*end*/;
				o.vertex = UnityObjectToClipPos(v.vertex);
				return o;
			}
			
			fixed4 frag (V2FData i /*ase_frag_input*/) : SV_Target
			{
				UNITY_SETUP_INSTANCE_ID( i );
				UNITY_SETUP_STEREO_EYE_INDEX_POST_VERTEX( i );
				fixed4 finalColor;
				/*ase_frag_code:i=v2f*/
				
				finalColor = /*ase_frag_out:Frag Color;Float4;_Color*/fixed4(1,1,1,1)/*end*/;
				return finalColor;
			}
			ENDCG
		}
	}
	CustomEditor "ASEMaterialInspector"
}
