use serde::Deserialize;

pub struct MappedDesign {}

impl<'de> Deserialize<'de> for MappedDesign {
	fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
	where
		D: serde::Deserializer<'de>,
	{
		todo!()
	}
}
