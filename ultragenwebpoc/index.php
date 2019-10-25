<?php
	$req = $_SERVER['REQUEST_URI'];
	if ($req == '/')
		$req = 'index';
	$gen = 'sources\\'.str_replace('/','\\',$req).'.gen';
	if (!file_exists($gen)) {
		$gen = 'sources\404.gen';
	}
	$temp = 'templates\post.ultra.html';
	$sep = $gen.' -separator:;';
	$live = '-l';
	$path = 'ultragen';
	$cmd = "$path -gens \"$sep\" -templates $temp $live 2>&1";
	$output = `$cmd`;
	echo $output;
?>