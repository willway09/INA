source("../../R/spreadstep.R")

test_that("Diagonal", {
	adjacency_matrix = matrix(
		data = c(
			1, 0, 0, 0, 0,
			0, 1, 0, 0, 0,
			0, 0, 1, 0, 0,
			0, 0, 0, 1, 0,
			0, 0, 0, 0, 1
		), byrow=T, nrow=5
	);

	state_vector = c(
		1,
		0,
		0,
		0,
		1
	);

	ans = spreadstep(adjacency_matrix, state_vector);

	expect_equal(ans, c(1, 0, 0, 0, 1));
})

test_that("One Link", {
	adjacency_matrix = matrix(
		data = c(
			1, 0, 0, 0, 1,
			0, 1, 0, 0, 0,
			0, 0, 1, 0, 0,
			0, 0, 0, 1, 0,
			0, 0, 0, 0, 1
		), byrow=T, nrow=5
	);

	state_vector = c(
		0,
		0,
		1,
		0,
		1
	);

	state_vector = spreadstep(adjacency_matrix, state_vector);
	expect_equal(state_vector, c(0, 0, 1, 0, 1));
	state_vector = spreadstep(adjacency_matrix, state_vector);
	expect_equal(state_vector, c(0, 0, 1, 0, 1));
})

test_that("Repeated 1", {
	adjacency_matrix = matrix(
		data = c(
			1, 1, 0, 0, 0,
			0, 1, 1, 0, 0,
			0, 0, 1, 1, 0,
			0, 0, 0, 1, 1,
			0, 0, 0, 0, 1
		), byrow=T, nrow=5
	);

	state_vector_1 = c(
		1,
		0,
		0,
		0,
		0
	);

	state_vector_1 = spreadstep(adjacency_matrix, state_vector_1);
	state_vector_1 = spreadstep(adjacency_matrix, state_vector_1);
	state_vector_1 = spreadstep(adjacency_matrix, state_vector_1);
	state_vector_1 = spreadstep(adjacency_matrix, state_vector_1);

	expect_equal(state_vector_1, c(1, 1, 1, 1, 1));

	state_vector_2 = c(
		0,
		0,
		0,
		0,
		1
	);

	state_vector_2 = spreadstep(adjacency_matrix, state_vector_2);
	state_vector_2 = spreadstep(adjacency_matrix, state_vector_2);
	state_vector_2 = spreadstep(adjacency_matrix, state_vector_2);
	state_vector_2 = spreadstep(adjacency_matrix, state_vector_2);

	expect_equal(state_vector_2, c(0, 0, 0, 0, 1));
})

test_that("Repeated 2", {
	adjacency_matrix = matrix(
		data = c(
			1, 1, 1, 0, 0,
			0, 1, 1, 1, 0,
			1, 0, 1, 0, 0,
			0, 0, 1, 1, 1,
			0, 0, 0, 0, 1
		), byrow=T, nrow=5
	);

	state_vector = c(1, 0, 0, 0, 0);
	state_vector = spreadstep(adjacency_matrix, state_vector);
	expect_equal(state_vector, c(1, 1, 1, 0, 0));
	state_vector = spreadstep(adjacency_matrix, state_vector);
	expect_equal(state_vector, c(1, 1, 1, 1, 0));
	state_vector = spreadstep(adjacency_matrix, state_vector);
	expect_equal(state_vector, c(1, 1, 1, 1, 1));
	state_vector = spreadstep(adjacency_matrix, state_vector);
	expect_equal(state_vector, c(1, 1, 1, 1, 1));

	state_vector = c(0, 1, 0, 0, 0);
	state_vector = spreadstep(adjacency_matrix, state_vector);
	expect_equal(state_vector, c(0, 1, 1, 1, 0));
	state_vector = spreadstep(adjacency_matrix, state_vector);
	expect_equal(state_vector, c(1, 1, 1, 1, 1));
	state_vector = spreadstep(adjacency_matrix, state_vector);
	expect_equal(state_vector, c(1, 1, 1, 1, 1));

	state_vector = c(0, 0, 1, 0, 0);
	state_vector = spreadstep(adjacency_matrix, state_vector);
	expect_equal(state_vector, c(1, 0, 1, 0, 0));
	state_vector = spreadstep(adjacency_matrix, state_vector);
	expect_equal(state_vector, c(1, 1, 1, 0, 0));
	state_vector = spreadstep(adjacency_matrix, state_vector);
	expect_equal(state_vector, c(1, 1, 1, 1, 0));
	state_vector = spreadstep(adjacency_matrix, state_vector);
	expect_equal(state_vector, c(1, 1, 1, 1, 1));

	state_vector = c(0, 0, 0, 1, 0);
	state_vector = spreadstep(adjacency_matrix, state_vector);
	expect_equal(state_vector, c(0, 0, 1, 1, 1));
	state_vector = spreadstep(adjacency_matrix, state_vector);
	expect_equal(state_vector, c(1, 0, 1, 1, 1));
	state_vector = spreadstep(adjacency_matrix, state_vector);
	expect_equal(state_vector, c(1, 1, 1, 1, 1));

	state_vector = c(0, 0, 0, 0, 1);
	state_vector = spreadstep(adjacency_matrix, state_vector);
	expect_equal(state_vector, c(0, 0, 0, 0, 1));
	state_vector = spreadstep(adjacency_matrix, state_vector);
	expect_equal(state_vector, c(0, 0, 0, 0, 1));
	state_vector = spreadstep(adjacency_matrix, state_vector);
	expect_equal(state_vector, c(0, 0, 0, 0, 1));
})

test_that("Repeated 3", {
	adjacency_matrix = matrix(
		data = c(
			1, 1, 1, 0, 0,
			0, 1, 1, 1, 0,
			0, 0, 1, 0, 0,
			0, 0, 1, 1, 1,
			0, 0, 0, 0, 1
		), byrow=T, nrow=5
	);

	state_vector = c(1, 0, 0, 0, 0);
	state_vector = spreadstep(adjacency_matrix, state_vector);
	expect_equal(state_vector, c(1, 1, 1, 0, 0));
	state_vector = spreadstep(adjacency_matrix, state_vector);
	expect_equal(state_vector, c(1, 1, 1, 1, 0));
	state_vector = spreadstep(adjacency_matrix, state_vector);
	expect_equal(state_vector, c(1, 1, 1, 1, 1));

	state_vector = c(0, 1, 0, 0, 0);
	state_vector = spreadstep(adjacency_matrix, state_vector);
	expect_equal(state_vector, c(0, 1, 1, 1, 0));
	state_vector = spreadstep(adjacency_matrix, state_vector);
	expect_equal(state_vector, c(0, 1, 1, 1, 1));
	state_vector = spreadstep(adjacency_matrix, state_vector);
	expect_equal(state_vector, c(0, 1, 1, 1, 1));

	state_vector = c(0, 0, 1, 0, 0);
	state_vector = spreadstep(adjacency_matrix, state_vector);
	expect_equal(state_vector, c(0, 0, 1, 0, 0));
	state_vector = spreadstep(adjacency_matrix, state_vector);
	expect_equal(state_vector, c(0, 0, 1, 0, 0));
	state_vector = spreadstep(adjacency_matrix, state_vector);
	expect_equal(state_vector, c(0, 0, 1, 0, 0));

	state_vector = c(0, 0, 0, 1, 0);
	state_vector = spreadstep(adjacency_matrix, state_vector);
	expect_equal(state_vector, c(0, 0, 1, 1, 1));
	state_vector = spreadstep(adjacency_matrix, state_vector);
	expect_equal(state_vector, c(0, 0, 1, 1, 1));
	state_vector = spreadstep(adjacency_matrix, state_vector);
	expect_equal(state_vector, c(0, 0, 1, 1, 1));

	state_vector = c(0, 0, 0, 0, 1);
	state_vector = spreadstep(adjacency_matrix, state_vector);
	expect_equal(state_vector, c(0, 0, 0, 0, 1));
	state_vector = spreadstep(adjacency_matrix, state_vector);
	expect_equal(state_vector, c(0, 0, 0, 0, 1));
	state_vector = spreadstep(adjacency_matrix, state_vector);
	expect_equal(state_vector, c(0, 0, 0, 0, 1));
})

test_that("Big", {
	adjacency_matrix = matrix(
		data = c(
			1, 0, 0, 0, 0, 0, 0, 0, 0, 0,
			0, 1, 0, 0, 0, 0, 0, 0, 0, 0,
			0, 0, 1, 0, 0, 0, 0, 0, 0, 0,
			0, 0, 1, 1, 0, 0, 0, 0, 0, 0,
			0, 0, 0, 0, 1, 0, 0, 0, 0, 0,
			0, 0, 0, 0, 0, 1, 0, 0, 0, 0,
			0, 0, 0, 0, 0, 0, 1, 0, 0, 0,
			0, 0, 0, 0, 0, 0, 0, 1, 0, 0,
			0, 0, 0, 0, 0, 0, 0, 1, 1, 0,
			0, 0, 0, 0, 0, 0, 0, 0, 0, 1
		), byrow=T, nrow=10
	);

	state_vector = c(1, 0, 0, 0, 0);
	expect_error(spreadstep(adjacency_matrix, state_vector));

	state_vector = c(1, 0, 0, 0, 1, 0, 0, 1, 0, 1);
	state_vector = spreadstep(adjacency_matrix, state_vector);
	expect_equal(state_vector, c(1, 0, 0, 0, 1, 0, 0, 1, 0, 1));
})
