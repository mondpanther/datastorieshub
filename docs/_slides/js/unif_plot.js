(function($) {
    $(document).ready(function() {
	
	$('#unif_plot').scianimator({
	    'images': ['unif_dir/unif_plot1.png', 'unif_dir/unif_plot2.png', 'unif_dir/unif_plot3.png', 'unif_dir/unif_plot4.png', 'unif_dir/unif_plot5.png', 'unif_dir/unif_plot6.png', 'unif_dir/unif_plot7.png', 'unif_dir/unif_plot8.png', 'unif_dir/unif_plot9.png', 'unif_dir/unif_plot10.png', 'unif_dir/unif_plot11.png', 'unif_dir/unif_plot12.png', 'unif_dir/unif_plot13.png', 'unif_dir/unif_plot14.png', 'unif_dir/unif_plot15.png', 'unif_dir/unif_plot16.png', 'unif_dir/unif_plot17.png', 'unif_dir/unif_plot18.png', 'unif_dir/unif_plot19.png', 'unif_dir/unif_plot20.png'],
	    'width': 600,
	    'delay': 1000,
	    'loopMode': 'loop'
	});
	$('#unif_plot').scianimator('play');
    });
})(jQuery);
