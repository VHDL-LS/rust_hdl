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
            let formatter = DesignUnitFormatter {
                formatter: self,
                tokens,
            };
            result.push_str(&formatter.format_any_design_unit(design_unit))
        }
        result
    }
}

impl DesignUnitFormatter<'_, '_> {
    pub fn format_any_design_unit(&self, unit: &AnyDesignUnit) -> String {
        use AnyDesignUnit::*;
        match unit {
            Primary(primary) => self.format_any_primary_unit(primary),
            Secondary(secondary) => self.format_any_secondary_unit(secondary),
        }
    }

    pub fn format_any_primary_unit(&self, unit: &AnyPrimaryUnit) -> String {
        use AnyPrimaryUnit::*;
        match unit {
            Entity(entity) => self.format_entity(entity),
            Configuration(configuration) => self.format_configuration(configuration),
            Package(package) => self.format_package(package),
            PackageInstance(package_instance) => self.format_package_instance(package_instance),
            Context(context) => self.format_context(context),
        }
    }

    pub fn format_any_secondary_unit(&self, unit: &AnySecondaryUnit) -> String {
        unimplemented!()
    }

    pub fn format_configuration(&self, configuration: &ConfigurationDeclaration) -> String {
        unimplemented!()
    }

    pub fn format_package(&self, package: &PackageDeclaration) -> String {
        unimplemented!()
    }

    pub fn format_package_instance(&self, instance: &PackageInstantiation) -> String {
        unimplemented!()
    }

    pub fn format_context(&self, context: &ContextDeclaration) -> String {
        unimplemented!()
    }
}
