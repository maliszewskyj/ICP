	MODULE vdata
	use iso_c_binding
	INTEGER(c_int),bind(C,name="vhisto")::vhisto(256)
	INTEGER(c_int),bind(C,name="nvhisto"):: nvhisto
	END MODULE vdata
