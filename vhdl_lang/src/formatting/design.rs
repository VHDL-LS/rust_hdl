use crate::ast::{
    AnyDesignUnit, AnyPrimaryUnit, AnySecondaryUnit, ConfigurationDeclaration, ContextDeclaration,
    DesignFile, PackageInstantiation,
};
use crate::formatting::Formatter;
use vhdl_lang::ast::PackageDeclaration;
use vhdl_lang::formatting::DesignUnitFormatter;

impl Formatter {
    pub fn format_design_file(&self, file: &DesignFile) -> String {
        let mut result = String::new();
        for (tokens, design_unit) in &file.design_units {
            let formatter = DesignUnitFormatter::new(tokens);
            formatter.format_any_design_unit(design_unit, &mut result);
        }
        result
    }
}

impl DesignUnitFormatter<'_> {
    pub fn format_any_design_unit(&self, unit: &AnyDesignUnit, buffer: &mut String) {
        use AnyDesignUnit::*;
        match unit {
            Primary(primary) => self.format_any_primary_unit(primary, buffer),
            Secondary(secondary) => self.format_any_secondary_unit(secondary, buffer),
        }
    }

    pub fn format_any_primary_unit(&self, unit: &AnyPrimaryUnit, buffer: &mut String) {
        use AnyPrimaryUnit::*;
        match unit {
            Entity(entity) => self.format_entity(entity, buffer),
            Configuration(configuration) => self.format_configuration(configuration, buffer),
            Package(package) => self.format_package(package, buffer),
            PackageInstance(package_instance) => {
                self.format_package_instance(package_instance, buffer)
            }
            Context(context) => self.format_context(context, buffer),
        }
    }

    pub fn format_any_secondary_unit(&self, unit: &AnySecondaryUnit, buffer: &mut String) {
        use AnySecondaryUnit::*;
        match unit {
            Architecture(architecture) => self.format_architecture(architecture, buffer),
            PackageBody(_) => unimplemented!(),
        }
    }

    pub fn format_configuration(
        &self,
        configuration: &ConfigurationDeclaration,
        buffer: &mut String,
    ) {
        unimplemented!()
    }

    pub fn format_package(&self, package: &PackageDeclaration, buffer: &mut String) {
        unimplemented!()
    }

    pub fn format_package_instance(&self, instance: &PackageInstantiation, buffer: &mut String) {
        unimplemented!()
    }

    pub fn format_context(&self, context: &ContextDeclaration, buffer: &mut String) {
        unimplemented!()
    }
}
